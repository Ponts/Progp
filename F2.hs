-- August Falk Pontus Brink

module F2 where

import Data.List
-- Datatyper
data MolSeq = MolSeq {seqName :: String, seqSequence :: String, seqType :: String}
data Profile= Profile {matrix :: [[(Char, Int)]], nrOfSequences :: Int, name :: String} 

-- Tar två strängar och avgör om det är en protein eller dna
-- Returnar MolSeq protein eller dna
string2seq :: String -> String -> MolSeq
string2seq namn sekvens = if isProtein sekvens
				then MolSeq namn sekvens "Protein" -- är Protein
				else MolSeq namn sekvens "DNA"

molseqs2profile :: String -> [MolSeq] -> Profile
molseqs2profile string mols = Profile (makeProfileMatrix mols) (length mols) string


nucleotides = "ACGT"
aminoacids = sort "ARNDCEQGHILKMFPSTWYVX"

makeProfileMatrix :: [MolSeq] -> [[(Char, Int)]]
makeProfileMatrix [] = error "Empty sequence list"
makeProfileMatrix sl = res
  where 
    t = seqType (head sl)
    defaults = 
      if (t == "DNA") then
        zip nucleotides (replicate (length nucleotides) 0) -- Rad (i) Skapar en lista av tuples med bokstav respektive nolla
      else 
        zip aminoacids (replicate (length aminoacids) 0)   -- Rad (ii) -||- samma sak fast med karaktärer för Protein
    strs = map seqSequence sl                              -- Rad (iii) strs är en lista som håller typen av molseq (DNA eller Protein)
    tmp1 = map (map (\x -> ((head x), (length x))) . group . sort)
               (transpose strs)                            -- Rad (iv) 
    equalFst a b = (fst a) == (fst b)
    res = map sort (map (\l -> unionBy equalFst l defaults) tmp1)

-- Kollar om det är en DNA.
isProtein :: String -> Bool
isProtein [] = False 				-- Kommer vi hit är det en Protein
isProtein (c : rest) 
				| elem c proteinChars = True	-- kommer vi hit är det en protein
				| otherwise = isProtein(rest)	-- Är en char, kolla nästa

-- Chars som finns i Protein
proteinChars = ['R','N','D','E','Q','H','I','L','K','M','F','P','S','W','Y','V']

-- Kollar längden på en sekvens
seqLength :: MolSeq -> Int
seqLength m = length (seqSequence m)

seqDistance :: MolSeq -> MolSeq -> Double
seqDistance m1 m2 = case molTypes m1 m2 of 
						(True, True)	-> calcProtein m1 m2
						(False, False)	-> calcDNA m1 m2
						_				-> error "Invalid types"
	
	
calcDNA :: MolSeq -> MolSeq -> Double
calcDNA m1 m2 = if hammingDist m1 m2 > 0.74
			then 3.3
			else -(3/4)*log(1-(4*hammingDist m1 m2)/3)
		
calcProtein :: MolSeq -> MolSeq -> Double
calcProtein m1 m2 = if hammingDist m1 m2 > 0.94
			then 3.7
			else -(19/20)*log(1-(20*hammingDist m1 m2)/19)
				
-- Tar in två sekvenser
-- Returnerar en Tuple med två Boolean som säger om sekvenserna är DNA eller inte				   
molTypes :: MolSeq -> MolSeq -> (Bool,Bool)
molTypes m1 m2 = (isProtein (seqSequence m1), isProtein (seqSequence m2))

-- Beränkar det normaliserade Hamming-avståndet mellan två sekvenser		
hammingDist :: MolSeq -> MolSeq -> Double
hammingDist m1 m2 = countDiff (seqSequence m1) (seqSequence m2) / fromIntegral (length (seqSequence m1))

-- Beränkar antaler element som skiljer sig mellan två strängar
countDiff :: String -> String -> Double
countDiff [] [] = 0.0 -- Strängarna är samma längd så detta funkar 
countDiff s1 s2
					| head s1 /= head s2 = 1.0 + countDiff (tail s1) (tail s2)
					| otherwise = countDiff (tail s1) (tail s2)

