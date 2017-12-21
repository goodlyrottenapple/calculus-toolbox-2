module Servant.JS.Custom where

import           Prelude
import           Control.Lens
import           Data.Maybe (isJust)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import           Data.Monoid
import           Servant.Foreign
import           Servant.JS.Internal


customJSWith :: CommonGeneratorOptions -> JavaScriptGenerator
customJSWith opts = mconcat . map (generateCustomJSWith opts)

-- | js codegen using XmlHttpRequest
generateCustomJSWith :: CommonGeneratorOptions -> AjaxReq -> Text
generateCustomJSWith opts req = "\n" <>
    fname <> " = function(port, " <> argsStr <> ") {\n"
 <> "  var xhr = new XMLHttpRequest();\n"
 <> "  xhr.open('" <> decodeUtf8 method <> "', " <> url <> ", true);\n"
 <>    reqheaders
 <> "  xhr.setRequestHeader('Cache-Control', 'no-cache');\n"
 <> "  xhr.setRequestHeader('Accept', 'application/json');\n"
 <> (if isJust (req ^. reqBody) then "  xhr.setRequestHeader('Content-Type', 'application/json');\n" else "")
 <> "  xhr.onreadystatechange = function () {\n"
 <> "    var res = null;\n"
 <> "    if (xhr.readyState === 4) {\n"
 <> "      if (xhr.status === 204 || xhr.status === 205) {\n"
 <> "        " <> onSuccess <> "();\n"
 <> "      } else if (xhr.status >= 200 && xhr.status < 300) {\n"
 <> "        try { res = JSON.parse(xhr.responseText); } catch (e) { " <> onError <> "(e); }\n"
 <> "        if (res) " <> onSuccess <> "(res);\n"
 <> "      } else {\n"
 <> "        try { res = JSON.parse(xhr.responseText); } catch (e) { " <> onError <> "(e); }\n"
 <> "        if (res) " <> onError <> "(res);\n"
 <> "      }\n"
 <> "    }\n"
 <> "  };\n"
 <> "  xhr.send(" <> dataBody <> ");\n"
 <> "};\n"

  where argsStr = T.intercalate ", " args
        args = captures
            ++ map (view $ queryArgName . argPath) queryparams
            ++ body
            ++ map ( toValidFunctionName
                   . (<>) "header"
                   . view (headerArg . argPath)
                   ) hs
            ++ [onSuccess, onError]

        captures = map (view argPath . captureArg)
                 . filter isCapture
                 $ req ^. reqUrl.path

        hs = req ^. reqHeaders

        queryparams = req ^.. reqUrl.queryStr.traverse

        body = if isJust(req ^. reqBody)
                 then [requestBody opts]
                 else []

        onSuccess = successCallback opts
        onError = errorCallback opts

        dataBody =
          if isJust (req ^. reqBody)
            then "JSON.stringify(body)"
            else "null"


        reqheaders =
          if null hs
            then ""
            else headersStr <> "\n"

          where
            headersStr = T.intercalate "\n" $ map headerStr hs
            headerStr header = "  xhr.setRequestHeader(\"" <>
              header ^. headerArg . argPath <>
              "\", " <> toJSHeader header <> ");"

        namespace = if moduleName opts == ""
                       then "var "
                       else (moduleName opts) <> "."
        fname = namespace <> (toValidFunctionName (functionNameBuilder opts $ req ^. reqFuncName))

        method = req ^. reqMethod
        url = if url' == "`" then "'/'" else url'
        url' = "`"
           <> urlPrefix opts
           <> urlArgs <> "`"
           <> queryArgs

        -- a bit of a hack but can't be bothered to find out where the final ' is appended
        urlArgs = T.dropEnd 1 $ jsSegments
                $ req ^.. reqUrl.path.traverse

        queryArgs = if null queryparams
                      then ""
                      else " + '?" <> jsParams queryparams