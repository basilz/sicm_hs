module Main (main) where

main :: IO ()
main = do
	putStrLn "hs-sicm command line interface"
	putStrLn "--------------------------------"
	putStrLn "Sample statistics for demo data:"
	putStrLn ""
	putStrLn "CLI options will be added in a future iteration."

demoData :: [Double]
demoData = [1, 3, 5, 7, 9]

renderMean :: [Double] -> IO ()
renderMean xs = undefined

