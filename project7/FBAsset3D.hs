{-|
A super quick proof of concept of upload Gltf asset to Facebook Asset3d and create a post
Testing on Haskell Curl, Network.Curl & Facebook API
-}

import Network.Curl
import System.Process
import Data.List.Split

data Gltf = Gltf { file ::String,
                  fallback_image :: String,
                  message :: String
                 } deriving (Show)
convertGltfToParam g = (concat ["-F file=",file g], concat ["-F fallback_image=",fallback_image g])

input :: [Gltf]
input = [Gltf {file="@/<FOLDER_PATH>/ddpmouse.glb", fallback_image="@/<FOLDER_PATH>/ddpmouse.jpg", message="Placeholder Message"}]

access_token :: String
access_token = "<REPLACE_YOUR_ACCESS_TOKEN>"

genEndPoint :: String -> String
genEndPoint feature = concat ["https://graph.facebook.com/v2.12/",
                              feature,
                              "?",
                              "&access_token=",
                              access_token]

postMessage :: String -> String -> IO ()
-- postMessage message asset3dId = curlPost (genEndPoint "me/feed/") [concat ["message=", message], concat ["asset3d_id=",asset3dId] ,"privacy={\"value\":\"SELF\"}"]
postMessage message asset3dId = curlPost (genEndPoint "me/feed/") [concat ["message=", message], concat ["asset3d_id=",asset3dId]]

upload3DAsset :: Gltf -> IO ()
upload3DAsset g = readProcess "curl" [pf, pfbi, genEndPoint "me/asset3ds"] ""
                                    >>= return.extract3DId >>= postMessage (message g)
                                    where (pf, pfbi) = convertGltfToParam g

-- Super dirty way to retrive the 3D_id
extract3DId :: String -> String
extract3DId str = takeWhile (\c -> c /= '"') ((splitOn "\":\"" str)!!1)

main :: IO ()
main = do
    mapM_ (upload3DAsset) input
    print "Fin"
