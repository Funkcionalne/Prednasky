module TestIteruj where

import Iteruj
--import Iteruj(iteruj, iteruj', iteruj'', iteruj''', iteruj'''', iteruj_foldr, iteruj_foldl, iteruj_using_iterate, iteruj_funkciu)
import Test.HUnit

main = do
	runTestTT $ TestList [ 
                TestList [ 
                  TestCase $ assertEqual "iteruj 5 (+4) 100" 120 ( iteruj 5 (+4) 100 ),
                  TestCase $ assertEqual "iteruj 5 (*4) 100 " 102400 ( iteruj 5 (*4) 100 ),
                  TestCase $ assertEqual "iteruj 5 (++ab) c " "cababababab"( iteruj 5 (++"ab") "c" )
                ],
                TestList [ 
                  TestCase $ assertEqual "iteruj' 5 (+4) 100" 120 ( iteruj' 5 (+4) 100 ),
                  TestCase $ assertEqual "iteruj' 5 (*4) 100 " 102400 ( iteruj' 5 (*4) 100 ),
                  TestCase $ assertEqual "iteruj' 5 (++ab) c " "cababababab"( iteruj' 5 (++"ab") "c" )
                ],
                TestList [ 
                  TestCase $ assertEqual "iteruj'' 5 (+4) 100" 120 ( iteruj'' 5 (+4) 100 ),
                  TestCase $ assertEqual "iteruj'' 5 (*4) 100 " 102400 ( iteruj'' 5 (*4) 100 ),
                  TestCase $ assertEqual "iteruj'' 5 (++ab) c " "cababababab"( iteruj'' 5 (++"ab") "c" )
                ],
                TestList [ 
                  TestCase $ assertEqual "iteruj''' 5 (+4) 100" 120 ( iteruj''' 5 (+4) 100 ),
                  TestCase $ assertEqual "iteruj''' 5 (*4) 100 " 102400 ( iteruj''' 5 (*4) 100 ),
                  TestCase $ assertEqual "iteruj''' 5 (++ab) c " "cababababab"( iteruj''' 5 (++"ab") "c" )
                ],
                TestList [ 
                  TestCase $ assertEqual "iteruj'''' 5 (+4) 100" 120 ( iteruj'''' 5 (+4) 100 ),
                  TestCase $ assertEqual "iteruj'''' 5 (*4) 100 " 102400 ( iteruj'''' 5 (*4) 100 ),
                  TestCase $ assertEqual "iteruj'''' 5 (++ab) c " "cababababab"( iteruj'''' 5 (++"ab") "c" )
                ],
                TestList [ 
                  TestCase $ assertEqual "iteruj_foldr 5 (+4) 100" 120 ( iteruj_foldr 5 (+4) 100 ),
                  TestCase $ assertEqual "iteruj_foldr 5 (*4) 100 " 102400 ( iteruj_foldr 5 (*4) 100 ),
                  TestCase $ assertEqual "iteruj_foldr 5 (++ab) c " "cababababab"( iteruj_foldr 5 (++"ab") "c" )
                ],
                TestList [ 
                  TestCase $ assertEqual "iteruj_foldl 5 (+4) 100" 120 ( iteruj_foldl 5 (+4) 100 ),
                  TestCase $ assertEqual "iteruj_foldl 5 (*4) 100 " 102400 ( iteruj_foldl 5 (*4) 100 ),
                  TestCase $ assertEqual "iteruj_foldl 5 (++ab) c " "cababababab"( iteruj_foldl 5 (++"ab") "c" )
                ],
                TestList [ 
                  TestCase $ assertEqual "iteruj_using_iterate 5 (+4) 100" 120 ( iteruj_using_iterate 5 (+4) 100 ),
                  TestCase $ assertEqual "iteruj_using_iterate 5 (*4) 100 " 102400 ( iteruj_using_iterate 5 (*4) 100 ),
                  TestCase $ assertEqual "iteruj_using_iterate 5 (++ab) c " "cababababab"( iteruj_using_iterate 5 (++"ab") "c" )
                ],
                TestList [ 
                  TestCase $ assertEqual "iteruj_funkciu 5 (+4) 100" 120 ( iteruj_funkciu 5 (+4) 100 ),
                  TestCase $ assertEqual "iteruj_funkciu 5 (*4) 100 " 102400 ( iteruj_funkciu 5 (*4) 100 ),
                  TestCase $ assertEqual "iteruj_funkciu 5 (++ab) c " "cababababab"( iteruj_funkciu 5 (++"ab") "c" )
                ]
              ]
