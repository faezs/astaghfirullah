{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Exception (throw)
import Control.Monad.Logger
import Data.Csv (FromNamedRecord (..), (.:))
import Data.Csv qualified as Csv
import Data.Generics.Sum.Any (AsAny (_As))
import Ema
import Ema.CLI qualified
import Ema.Route.Generic.TH
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import GHC.IO.Exception (userError)
import Match qualified
import Optics.Core (Prism', (%))
import Options.Applicative
import Shower qualified
import System.FilePath ((</>))
import System.UnionMount qualified as UM
import Text.Blaze.Html.Renderer.Utf8 qualified as RU
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import UnliftIO

data ImpactAssessment = ImpactAssessment
  { iaDistrict :: Text
  , iaTehsil :: Text
  , iaPopulation :: Text
  }
  deriving stock (Eq, Show)

instance FromNamedRecord ImpactAssessment where
  parseNamedRecord r =
    ImpactAssessment
      <$> r .: "District"
      <*> r .: "Tehsil"
      <*> r .: "Total Population"

impactAssessmentDynamic ::
  forall m.
  (MonadIO m, MonadUnliftIO m, MonadLogger m, MonadLoggerIO m) =>
  -- | Path to `./data` directory
  FilePath ->
  m (Dynamic m [ImpactAssessment])
impactAssessmentDynamic dataDir = do
  let pats = [((), "*.csv")]
  Dynamic <$> UM.mount dataDir pats mempty mempty handleAction
  where
    handleAction ::
      () ->
      FilePath ->
      UM.FileAction () ->
      m ([ImpactAssessment] -> [ImpactAssessment])
    handleAction () fp fpAct = do
      if fp == "impact-assesment.csv"
        then case fpAct of
          UM.Refresh _ () -> do
            logInfoNS "csv" $ "Reading " <> toText fp
            s <- readFileLBS $ dataDir </> fp
            case Csv.decodeByName @ImpactAssessment s of
              Left err -> throw $ userError $ "Failed to decode CSV: " <> fp <> ": " <> err
              Right (_, toList -> rows) ->
                pure $ \_ -> rows
          UM.Delete -> do
            pure $ \_ -> []
        else pure id

data Model = Model
  { modelBaseUrl :: Text
  , modelStatic :: SR.Model
  , modelImpactAssessment :: [ImpactAssessment]
  }
  deriving stock (Eq, Show, Generic)

data HtmlRoute
  = HtmlRoute_Index
  | HtmlRoute_About
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)

deriveGeneric ''HtmlRoute
deriveIsRoute ''HtmlRoute [t|'[]|]

type StaticRoute = SR.StaticRoute "static"

data Route
  = Route_Html HtmlRoute
  | Route_Static StaticRoute
  deriving stock (Eq, Show, Ord, Generic)

deriveGeneric ''Route
deriveIsRoute
  ''Route
  [t|
    [ -- To render a `Route` we need `Model`
      WithModel Model
    , -- Override default sub-route encoding (to avoid the folder prefix in encoded URLs)
      WithSubRoutes [HtmlRoute, StaticRoute]
    ]
    |]

instance EmaSite Route where
  type SiteArg Route = CliArgs
  siteInput cliAct args = do
    staticRouteDyn <- siteInput @StaticRoute cliAct ()
    impactAssessment <- impactAssessmentDynamic "data"
    pure $ Model (cliArgsBaseUrl args) <$> staticRouteDyn <*> impactAssessment
  siteOutput rp m = \case
    Route_Html r ->
      pure $ Ema.AssetGenerated Ema.Html $ renderHtmlRoute rp m r
    Route_Static r ->
      siteOutput (rp % (_As @"Route_Static")) (modelStatic m) r

renderHtmlRoute :: Prism' FilePath Route -> Model -> HtmlRoute -> LByteString
renderHtmlRoute rp m r = do
  RU.renderHtml $ do
    H.docType
    H.html ! A.lang "en" $ do
      H.head $ do
        renderHead rp m r
      H.body $ do
        renderBody rp m r

renderHead :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderHead rp model r = do
  H.meta ! A.charset "UTF-8"
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  H.title $ H.toHtml $ routeTitle r <> " - Ema Template"
  H.base ! A.href (H.toValue $ modelBaseUrl model)
  H.link ! A.rel "stylesheet" ! A.href (staticRouteUrl rp model "tailwind.css")

renderBody :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderBody rp model r = do
  H.div ! A.class_ "container mx-auto mt-8 p-2" $ do
    renderNavbar rp r
    H.h1 ! A.class_ "text-3xl font-bold" $ H.toHtml $ routeTitle r
    case r of
      HtmlRoute_Index -> do
        H.pre $ H.toHtml $ Shower.shower $ modelImpactAssessment model
      HtmlRoute_About -> do
        "You are on the about page."
        "Go to "
        routeLink rp HtmlRoute_About "Index"
    H.img ! A.src (staticRouteUrl rp model "logo.svg") ! A.class_ "py-4 w-32" ! A.alt "Ema Logo"

renderNavbar :: Prism' FilePath Route -> HtmlRoute -> H.Html
renderNavbar rp currentRoute =
  H.nav ! A.class_ "w-full text-xl font-bold flex space-x-4  mb-4" $ do
    forM_ (universe @HtmlRoute) $ \r ->
      let extraClass = if r == currentRoute then "bg-rose-400 text-white" else "text-gray-700"
       in H.a ! A.href (H.toValue $ routeUrl rp $ Route_Html r)
            ! A.class_ ("rounded p-2 " <> extraClass)
            $ H.toHtml $ routeTitle r

routeTitle :: HtmlRoute -> Text
routeTitle r = case r of
  HtmlRoute_Index -> "Home"
  HtmlRoute_About -> "About"

routeLink :: Prism' FilePath Route -> HtmlRoute -> H.Html -> H.Html
routeLink rp r =
  H.a ! A.href (H.toValue $ routeUrl rp $ Route_Html r)
    ! A.class_ "text-rose-400"

-- | Link to a file under ./static
staticRouteUrl :: IsString r => Prism' FilePath Route -> Model -> FilePath -> r
staticRouteUrl rp m =
  SR.staticRouteUrl (rp % (_As @"Route_Static")) (modelStatic m)

-- CLI argument handling
-- ---------------------

data CliArgs = CliArgs
  { cliArgsBaseUrl :: Text
  , cliArgsEmaCli :: Ema.CLI.Cli
  }
  deriving stock (Eq, Show)

parseCliArgs :: IO CliArgs
parseCliArgs =
  execParser $ parserInfo cliParser
  where
    cliParser :: Parser CliArgs
    cliParser =
      CliArgs
        <$> (option str $ long "base-url" <> metavar "BASE_URL" <> help "Base URL to use in <base>" <> value "/")
        <*> Ema.CLI.cliParser
    parserInfo :: Parser a -> ParserInfo a
    parserInfo p =
      info
        (versionOption <*> p <**> helper)
        ( fullDesc
            <> progDesc "astaghfirullah: TODO"
            <> header "astaghfirullah"
        )
      where
        versionOption = infoOption "0.1" (long "version" <> help "Show version")

-- Main entrypoint
-- ---------------

main :: IO ()
main = do
  cliArgs <- parseCliArgs
  void $ Ema.runSiteWithCli @Route (cliArgsEmaCli cliArgs) cliArgs
