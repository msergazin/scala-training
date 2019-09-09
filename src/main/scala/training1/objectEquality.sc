val list1 = List(1,2,3)
val list2 = List(1,2,3,4)
list1 == list2
(list1.::(4))
list1:+4
list1.appended(4)
list1:+4 == list2

val ref1 = List(2)
val ref2 = ref1
ref1 == ref2
ref1.eq(ref2)

list1.ne(list2)