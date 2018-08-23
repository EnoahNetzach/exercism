module School (School, add, empty, grade, sorted) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Grade = Int
type Student = String
type Students = S.Set Student
type School = M.Map Grade Students

f :: Student -> Maybe Students -> Maybe Students
f student (Just students) = Just $ S.insert student students
f student Nothing         = Just $ S.fromList [student]

add :: Grade -> Student -> School -> School
add gradeNum student = M.alter (f student) gradeNum

empty :: School
empty = M.empty

grade :: Grade -> School -> [Student]
grade gradeNum school = S.toAscList $ M.findWithDefault S.empty gradeNum school

sorted :: School -> [(Grade, [Student])]
sorted school = M.toList $ M.map S.toAscList school
