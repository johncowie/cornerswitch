import Test.HUnit

test1 = TestCase (assertEqual "Checking 2+2=5" (2+2) 5)
tests = TestList [TestLabel "test1" test1]

main = do
	counts <- runTestTT tests
	return counts
