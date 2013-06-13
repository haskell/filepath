
test('filepath',
     [reqlib('QuickCheck'),
      extra_clean(['FilePath_Test.hs',
                   'FilePath_Test.hi', 'FilePath_Test.o',
                   'FilePath_Test',    'FilePath_Test.exe',
                   'GenTests.hi',      'GenTests.o',
                   'GenTests',         'GenTests.exe'])],
     run_command, ['$MAKE -s --no-print-directory ghc-filepath-tests'])

