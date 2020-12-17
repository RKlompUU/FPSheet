module UnitTests where

import Test.Hspec

import Sheet.Backend.Standard.Deps


allUnitTests :: IO ()
allUnitTests = hspec $ do
  describe "dependency resolver" $ do
    it "resolves to root only if no dependencies" $ do
      resolveDeps [
        ((0,0),[])
        ] `shouldBe` [(0,0)]
    it "resolves to root + root's dependencies (if no further deps)" $ do
      resolveDeps [
        ((0,0),[(0,1)]),
        ((0,1),[])
        ] `shouldBe` [(0,0),(0,1)]
      resolveDeps [
        ((0,0),[(0,1),(0,2)]),
        ((0,1),[]),
        ((0,2),[])
        ] `shouldSatisfy` \res ->
             res == [(0,0),(0,1),(0,2)]
          || res == [(0,0),(0,2),(0,1)]
    it "resolves to root + nested dependencies (simple: acyclic)" $ do
      resolveDeps [
        ((0,0),[(0,1)]),
        ((0,2),[(1,0)]),
        ((1,0),[])
        ] `shouldBe` [(0,0),(0,2),(1,0)]
      resolveDeps [
        ((0,0),[(0,1)]),
        ((0,2),[(1,0),(2,0)]),
        ((1,0),[]),
        ((2,0),[])
        ] `shouldSatisfy` \res ->
             res == [(0,0),(0,2),(1,0),(2,0)]
          || res == [(0,0),(0,2),(2,0),(1,0)]
    it "cyclic: intra looping cell is ignored" $ do
      resolveDeps [
        ((0,0),[(0,0)])
        ] `shouldBe` []
      resolveDeps [
        ((0,0),[(0,0)]),
        ((0,1),[])
        ] `shouldBe` [(0,1)]
      resolveDeps [
        ((0,0),[(0,1),(0,0)]),
        ((0,1),[])
        ] `shouldBe` []
    it "cyclic: inter looping cells are " $ do
      resolveDeps [
        ((0,0),[(0,1)]),
        ((0,1),[(0,0)])
        ] `shouldBe` []
      resolveDeps [
        ((0,0),[(0,1)]),
        ((0,1),[(0,0)]),
        ((0,2),[])
        ] `shouldBe` [(0,2)]
