module Mid.Db.Query where

import Database.Relational.Query hiding (asc, desc, wheres)
import qualified Database.Relational.Query as Query
import Database.Relational.Query.Monad.Trans.Ordering (Orderings)
-- import Database.Relational.Query.Projectable (OperatorProjectable)
-- import Database.Relational.Query.ProjectableExtended (AggregatedContext)


asc_ :: Monad m
     => Pi a t
     -> Projection c a
     -> Orderings c m (Projection c a)
asc_ pi proj = do
  Query.asc $ proj ! pi
  return proj

desc_ :: Monad m
      => Pi a t
      -> Projection c a
      -> Orderings c m (Projection c a)
desc_ pi proj = do
  Query.desc $ proj ! pi
  return proj

-- where_ :: ( MonadRestrict Flat m
--           , ShowConstantTermsSQL t
--           , Database.Relational.Query.Projectable.OperatorProjectable p
--           -- , OperatorProjectable p
--           )
--        => Pi a b
--        -> (Projection c b -> p t -> Projection Flat (Maybe Bool))
--        -> t
--        -> Projection c a
--        -> m (Projection c a)
where_ pi op val proj = do
  Query.wheres $ op (proj ! pi) (Query.value val)
  return proj

-- whereNull_ :: ( MonadRestrict Flat m
--               , Database.Record.KeyConstraint.HasColumnConstraint NotNull r
--               )
--            => Pi a (Maybe r)
--            -> Projection Flat a
--            -> m (Projection Flat a)
whereNull_ pi proj = do
  Query.wheres $ Query.isNothing (proj ! pi)
  return proj

-- count_ :: ( Monad m
--           , Integral b
--           , Database.Relational.Query.ProjectableExtended.AggregatedContext ac
--           -- , AggregatedContext ac
--           , SqlProjectable (p ac)
--           )
--        => Pi a1 a
--        -> Projection Flat a1
--        -> m (p ac b)
count_ pi proj = return $ Query.count $ proj ! pi
