{-# LANGUAGE Strict #-}
module LambdaEval
  ( Fresh
  , fresh
  , runFresh
  , freshForConflicting
  , reduceOrConvert
  , runNextNormal
  , runNextRandom
  , runAll
  , randomItem
  , example1
  , expr1
  )

  where


import           Control.Monad.State
import qualified Data.Text as Text
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Set (Set)
import           System.Random
import           LambdaAST
import           LambdaFBAnn

-------------------------------------------------------------------------------
example1 :: String
example1 = "let true = \\t.\\f.t\nlet false = \\t.\\f.f\nlet not = \\p.@@p false true\nlet and = \\a.\\b.@@a b false\nlet or = \\a.\\b.@@a true b\nlet xor = \\a.\\b.@@a @@b false true b\nlet if = \\p.\\a.\\b.@@p a b\n\nlet zero = \\f.\\x.x\nlet succ = \\n.\\f.\\x.@f @@n f x\nlet add = \\m.\\n.\\f.\\x.@@m f @@n f x\nlet mult = \\m.\\n.\\f.@m@n f\nlet pred = \\n.\\f.\\x.@@@n \\g.\\h.@h@g f \\u.x \\u.u\nlet minus = \\m.\\n.@@n pred m\n\n\nlet iszero = \\n.@@n \\x.false true\nlet lesseq = \\m.\\n.@ iszero @@minus m n\nlet equals = \\m.\\n. @@and @@lesseq m n @@lesseq n m\n\nlet pair = \\x.\\y.\\z.@@z x y\nlet fst = \\p.@p \\x.\\y.x\nlet snd = \\p.@p \\x.\\y.y\n\nlet nil = @@pair true true\nlet isnil = fst\nlet cons = \\h.\\t.@@pair false @@pair h t\nlet head = \\z.@fst @snd z\nlet tail = \\z.@snd @snd z\n\nlet one = @succ zero\nlet two = @succ @succ zero\nlet three = @succ two\nlet four = @succ three\nlet five = @succ four\nlet six = @succ five\nlet nine = @@mult @@add one two three\nlet twelve = @@mult @@minus nine three two\n\nlet fix = \\f.@ \\x.@f @x x \\x.@f @x x\n\nlet div = \\c.\\n.\\m.\\f.\\x. @ \\d.@@@iszero d @@zero f x @f @@@@c d m f x @@minus n m\nlet divide = \\n.@@fix div @succ n\n\n\nlet fold = \\fold.\\f.\\x.\\list.@@@if @isnil list x @@f @head list @@@fold f x @tail list\nlet sum = \\sum.\\list. @@@if @isnil list zero @@add @head list @sum @tail list\nlet fak = \\f.\\n.@@@if @iszero n one @@mult n @f@pred n\n\nlet list1 = @@cons one @@cons two @@cons three @@cons twelve @@cons two nil\n\n#let main = @@@@fix fold mult one list1\n#@@main nachfolger nix\n@@@@divide six two nachfolger nix\n"
--example1 = "@ \\true.@ \\false.@ \\not.@ \\and.@ \\or.@ \\xor.@ \\if.@ \\zero.@ \\succ.@ \\add.@ \\mult.@ \\pred.@ \\minus.@ \\iszero.@ \\lesseq.@ \\equals.@ \\pair.@ \\fst.@ \\snd.@ \\nil.@ \\isnil.@ \\cons.@ \\head.@ \\tail.@ \\one.@ \\two.@ \\three.@ \\nine.@ \\twelve.@ \\fix.@ \\fold.@ \\sum.@ \\fak.@ \\list1.@ \\main1.@ \\main2.@ \\main3.@ @ main3 nachfolger nix @ @ @ @ fix fold mult one list1 @ @ fix sum list1 @ @ @ if @ @ equals twelve @ @ add @ head @ tail @ tail list1 nine yes no @ @ cons one @ @ cons two @ @ cons three @ @ cons three @ @ cons two nil \\f.\\n.@ @ @ if @ iszero n one @ @ mult n @ f @ pred n \\sum.\\list.@ @ @ if @ isnil list zero @ @ add @ head list @ sum @ tail list \\fold.\\f.\\x.\\list.@ @ @ if @ isnil list x @ @ f @ head list @ @ @ fold f x @ tail list \\f.@ \\x.@ f @ x x \\x.@ f @ x x @ @ mult @ @ minus nine three two @ @ mult @ @ add one two three @ succ two @ succ @ succ zero @ succ zero \\z.@ snd @ snd z \\z.@ fst @ snd z \\h.\\t.@ @ pair false @ @ pair h t fst @ @ pair true true \\p.@ p \\x.\\y.y \\p.@ p \\x.\\y.x \\x.\\y.\\z.@ @ z x y \\m.\\n.@ @ and @ @ lesseq m n @ @ lesseq n m \\m.\\n.@ iszero @ @ minus m n \\n.@ @ n \\x.false true \\m.\\n.@ @ n pred m \\n.\\f.\\x.@ @ @ n \\g.\\h.@ h @ g f \\u.x \\u.u \\m.\\n.\\f.@ m @ n f \\m.\\n.\\f.\\x.@ @ m f @ @ n f x \\n.\\f.\\x.@ f @ @ n f x \\f.\\x.x \\p.\\a.\\b.@ @ p a b \\a.\\b.@ @ a @ @ b false true b \\a.\\b.@ @ a true b \\a.\\b.@ @ a b false \\p.@ @ p false true \\t.\\f.f \\t.\\f.t"

-------------------------------------------------------------------------------
expr1 :: Expr ()
expr1 =
  App () (Abs () "q" (Var () "q")) (App () (App () (Abs () "x" (Abs () "y" (App () (Var () "y") (Var () "x")))) (Abs () "a" (Var () "a"))) (Var () "b"))

-------------------------------------------------------------------------------
type Fresh a = State Int a


fresh :: Text.Text -> Fresh Text.Text
fresh name = do
  c <- get
  put (c+1)
  return (Text.concat ["_", name, "_", Text.pack (show c)])


runFresh :: Fresh a -> Int -> (a, Int)
runFresh = runState


freshForConflicting :: Set Name -> Fresh (Map Name Name)
freshForConflicting free =
  Map.fromList <$> mapM (\name -> fresh name >>= \name' -> return (name, name')) (Set.toList free)

-------------------------------------------------------------------------------
reduceOrConvert :: Expr FBAnn -> Fresh (Expr FBAnn)
reduceOrConvert expr =
  case expr of
    App ann (Abs ann2 name body) arg ->
      if Set.null (conflictAnn ann) then
        return $ replace name arg body
      else do
        nameMap <- freshForConflicting (conflictAnn ann)
        let body' = renameLambdas nameMap body
        return (App ann (Abs ann2 name body') arg)
        --return $ replace name arg body'
    _ ->
      return expr


--runNext :: Int -> Expr FBAnn -> Int -> (Expr FBAnn, Int, Bool)
--runNext 0 expr counter = (expr, counter, True)
--runNext n expr counter =
--  case collectPaths [] expr of
--    path : _ ->
--      let
--        (expr', counter') =
--          --runState (applyAtAndAnno (reverse path) reduceOrConvert expr) counter
--          runState (applyAt (reverse path) reduceOrConvert expr) counter
--      in
--        runNext (n-1) (annoFBAnn expr') counter'
--        --runNext (n-1) expr' counter'
--    _ ->
--      (expr, counter, False)

-------------------------------------------------------------------------------
runNextNormal :: Int -> Expr FBAnn -> Int -> (Expr FBAnn, Int, Bool)
runNextNormal 0 expr counter = (expr, counter, True)
runNextNormal n expr counter =
  case collectPaths [] expr of
    path : _ ->
      let
        (expr', counter') =
          runState (applyAtAndAnno (reverse path) reduceOrConvert expr) counter
      in
        runNextNormal (n-1) expr' counter'
    _ ->
      (expr, counter, False)


runNextRandom :: StdGen -> Int -> Expr FBAnn -> Int -> (Expr FBAnn, Int, StdGen)
runNextRandom gen 0 expr counter = (expr, counter, gen)
runNextRandom gen n expr counter =
  case randomItem gen (collectPaths [] expr) of
    Nothing ->
      (expr, counter, gen)
    Just (path, gen') ->
      let
        (expr', counter') =
          runState (applyAtAndAnno (reverse path) reduceOrConvert expr) counter
      in
        runNextRandom gen' (n-1) expr' counter'


--runNext1Info :: Expr FBAnn -> Int -> (Expr FBAnn, Int, Bool)
--runNext1Info expr counter =
--  case collectPaths [] expr of
--    path : _ ->
--      let
--        (expr', counter') =
--          runState (applyAt (reverse path) reduceOrConvert expr) counter
--      in
--        (expr', counter', True)
--    _ ->
--      (expr, counter, False)

runAll :: Expr FBAnn -> Int -> (Expr FBAnn, Int)
runAll expr counter =
  case collectPaths [] expr of
    path : _ ->
      let
        (expr', counter') =
          runState (applyAtAndAnno (reverse path) reduceOrConvert expr) counter
      in
        --runAll (annoFBAnn expr') counter'
        runAll expr' counter'
    _ ->
      (expr, counter)


--runAll' :: Expr FBAnn -> Int -> [Expr FBAnn]
--runAll' expr counter =
--  case collectPaths [] expr of
--    path : _ ->
--      let
--        (expr', counter') =
--          runState (applyAt (reverse path) reduceOrConvert expr) counter
--      in
--        expr' : runAll' (annoFBAnn expr') counter'
--    _ ->
--      []
--

--runAllRandom :: StdGen -> Expr FBAnn -> Int -> [Expr FBAnn]
--runAllRandom gen expr counter =
--  case randomItem gen (collectPaths [] expr) of
--    Nothing ->
--      []
--    Just (path, gen') ->
--      let
--        (expr', counter') =
--          runState (applyAt (reverse path) reduceOrConvert expr) counter
--      in
--        expr' : runAllRandom gen' (annoFBAnn expr') counter'


--runAllRandom' :: StdGen -> Expr FBAnn -> Int -> [Expr FBAnn]
--runAllRandom' gen expr counter =
--  case randomItem gen (take 4 $ collectPaths [] expr) of
--    Nothing ->
--      []
--    Just (path, gen') ->
--      let
--        (expr', counter') =
--          runState (applyAt (reverse path) reduceOrConvert expr) counter
--      in
--        expr' : runAllRandom' gen' (annoFBAnn expr') counter'


--runNext' :: StdGen -> Int -> Expr FBAnn -> Int -> (Expr FBAnn, Int, StdGen)
--runNext' gen 0 expr counter = (expr, counter, gen)
--runNext' gen n expr counter =
--  case randomItem gen (collectPaths [] expr) of
--    Nothing ->
--      (expr, counter, gen)
--    Just (path, gen') ->
--      let
--        (expr', counter') =
--          runState (applyAt (reverse path) reduceOrConvert expr) counter
--      in
--        runNext' gen' (n-1) (annoFBAnn expr') counter'

-------------------------------------------------------------------------------
randomItem :: StdGen -> [a] -> Maybe (a, StdGen)
randomItem _ [] =
  Nothing
randomItem gen items =
  let
    (ix, gen') =
      randomR (0, length items - 1) gen
  in
    Just (head $ drop ix items, gen')


