module Part4.Tasks where

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (initA :< lastA) = lastA : reversed initA

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist [] = REmpty
listToRlist lst = listToRlist (init lst) :< last lst


-- Реализуйте все представленные ниже классы (см. тесты)

-- список без [] - скобок
showBracketlessList :: (Show a) => ReverseList a -> String
showBracketlessList REmpty = ""
showBracketlessList (REmpty :< lastA) = show lastA
showBracketlessList (initA :< lastA) = prev ++ sep ++ curr where
    prev = showBracketlessList initA
    sep = ","
    curr = show lastA

instance (Show a) => Show (ReverseList a) where
    show REmpty = "[]"
    show a = "[" ++ showBracketlessList a ++ "]"
    showsPrec _ x s = show x ++ s

instance (Eq a) => Eq (ReverseList a) where
    REmpty == REmpty = True
    (initA :< lastA) == (initB :< lastB) = (initA == initB) && (lastA == lastB)
    _ == _ = False
    left /= right = not (left == right)

-- Упадёт при "rHead REmpty", аналогично попытке выполнить "head []" - думаю, таков путь
-- N.B. при попытке реализации не через pattern matching, а через if (initA == REmpty) требует (Eq a) в типах,
-- Что повлекло бы за собой перетаскивание (Eq a) во все реализации классов, что не круто
rHead :: ReverseList a -> a
rHead (REmpty :< lastA) = lastA
rHead (initA :< _) = rHead initA

rReverse :: ReverseList a -> ReverseList a -> ReverseList a
rReverse REmpty _ = REmpty
rReverse (REmpty :< lastA) listB = listB :< lastA
rReverse (initA :< lastA) listB = rReverse initA (listB :< lastA)

collectReversedTail :: ReverseList a -> ReverseList a -> ReverseList a
collectReversedTail REmpty _ = REmpty
collectReversedTail (REmpty :< _) listB = listB
collectReversedTail (initA :< lastA) listB = collectReversedTail initA (listB :< lastA)

rTail :: ReverseList a -> ReverseList a
rTail REmpty = REmpty
rTail listA = rReverse (collectReversedTail listA REmpty) REmpty

joinLists :: ReverseList a -> ReverseList a -> ReverseList a
joinLists REmpty listA = listA
joinLists listA REmpty = listA
joinLists listA listB =
    joinLists (listA :< moveEl) newListB where
        moveEl = rHead listB
        newListB = rTail listB

instance Semigroup (ReverseList a) where
    REmpty <> a = a
    a <> REmpty = a
    listA <> listB = joinLists listA listB

instance Monoid (ReverseList a) where
    mempty = REmpty
    mappend = (<>)
    
instance Functor ReverseList where
    fmap _ REmpty = REmpty
    fmap f (initA :< lastA) = fmap f initA :< f lastA 
    
instance Applicative ReverseList where
    pure a = REmpty :< a
    _ <*> REmpty = REmpty
    REmpty <*> _ = REmpty :: (ReverseList b)
    (initF :< lastF) <*> listA = (initF <*> listA) <> fmap lastF listA
 
instance Monad ReverseList where
    REmpty >>= _ = REmpty
    (initA :< lastA) >>= f = (initA >>= f) <> f lastA
