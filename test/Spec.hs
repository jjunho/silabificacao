import           Syllable.Core
import           Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Syllable.Core" $ do
      describe "normalize" $ do
        it "converts to lowercase" $ do
          normalize "PALAVRA" `shouldBe` "palavra"
        it "handles mixed case" $ do
          normalize "PaLaVrA" `shouldBe` "palavra"
      describe "toPhonemes" $ do
        it "converts simple word to phonemes" $ do
          toPhonemes "casa"
            `shouldBe` [Consonant 'c', Vowel 'a', Consonant 's', Vowel 'a']
        it "handles accented vowels" $ do
          toPhonemes "café"
            `shouldBe` [Consonant 'c', Vowel 'a', Consonant 'f', Vowel 'é']
      describe "syllabify" $ do
        it "separates simple words correctly" $ do
          syllabify "casa"
            `shouldBe` Right
                         [ Syllable [Consonant 'c', Vowel 'a']
                         , Syllable [Consonant 's', Vowel 'a']
                         ]
        it "handles perfect clusters" $ do
          syllabify "prato"
            `shouldBe` Right
                         [ Syllable [Consonant 'p', Consonant 'r', Vowel 'a']
                         , Syllable [Consonant 't', Vowel 'o']
                         ]
        it "handles exception words" $ do
          syllabify "tungstênio"
            `shouldBe` Right
                         [ Syllable
                             [ Consonant 't'
                             , Vowel 'u'
                             , Consonant 'n'
                             , Consonant 'g'
                             , Consonant 's'
                             ]
                         , Syllable [Consonant 't', Vowel 'ê']
                         , Syllable [Consonant 'n', Vowel 'i']
                         , Syllable [Vowel 'o']
                         ]
        it "handles pterodáctilo correctly" $ do
          syllabify "pterodáctilo"
            `shouldBe` Right
                         [ Syllable [Consonant 'p', Consonant 't', Vowel 'e']
                         , Syllable [Consonant 'r', Vowel 'o']
                         , Syllable [Consonant 'd', Vowel 'á', Consonant 'c']
                         , Syllable [Consonant 't', Vowel 'i']
                         , Syllable [Consonant 'l', Vowel 'o']
                         ]
        --
