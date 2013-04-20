学习使用lisp获取豆瓣信息

* * * * *

#### 接口说明
*     fetch-following(uid &optional (start 0) (count 100))  
  获取用户following的豆友。  
  
*     fetch-followers(uid &optional (start 0) (count 100))  
  获取用户的followers。  
  
*     fetch-book-collection(uid &optional (start 0) (count 100))  
  获取用户收藏的图书。  
  
*     get-people(uid)  
  获取用户所有的following和followers。  
  
*     get-book-collection(uid)  
  获取用户收藏的所有图书。  
  
*     find-common-books(uid people)  
  查找用户和别人收藏的相同的书籍。
