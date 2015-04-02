import System.Random

q :: [[String]]
q = [["Bildet die", "Bildet das Erzeugendensystem einer", "Hat die Rechtskürzungsregel in jeder", "Hat in einer abelschen Gruppe jede", "Gibt es einen Homomorphismus in jede"],
	 ["Untergruppe", "Obergruppe", "Äquivalenzrelation", "Basis", "Bijektion"],
	 ["eines Vektorraumisomorphismus", "eines Ordnungshomomorphismus", "eines Nullvektorraums", "eines Nullteilers"],
	 ["triviale Faktorgruppen?", "Unterringe mit Einselement?", "kommutative Potenzmengen?", "gleichmächtige Schnittmengen?", "Äquivalenzklassen?"]]

generate :: [Int] -> String
generate [] 		 = ""
generate list@(x:xs) = (q !! (4 - length list) !! x) ++ " " ++ generate xs

rand :: StdGen -> [Int]
rand gen = let 
				(firstCoin, newGen)   = random gen
				(secondCoin, newGen') = random newGen
				(thirdCoin, newGen'') = random newGen'
				(forthCoin, newGen''') = random newGen''
       	   in
       	    	[firstCoin `mod` 5, secondCoin `mod` 5, thirdCoin `mod` 5, forthCoin `mod` 5]

main :: IO ()
main = newStdGen >>= \g -> putStrLn $ "Prof. Dr. Bernhard Steffen: " ++ (generate $ rand g)