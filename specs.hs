import Test.Hspec
import Track

main = hspec $ do 
    describe "start" $ do
        it "should put a new task in the list" $ do
            let tl = newTaskList
            start "TOF" 4807 (2018,04,15,19,40) tl
             `shouldBe` Right [(4807,"TOF",Start,(2018,04,15,19,40))]
        it "should not allow a duplicate start" $ do
            let tl = start "TOF" 4807 (2018,04,15,19,40) newTaskList
            (tl >>= start "TOF" 4807 (2018,04,15,21,30))
             `shouldBe` Left "Task already started"
