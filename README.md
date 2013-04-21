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
  
  
* * * * *

#### Examples(在sbcl下使用）

*1.加载package*  
  ;使用quickload加载

    * (ql:quickload :douban-reader)
    To load "douban-reader":
    Load 1 ASDF system:
    douban-reader
    ; Loading "douban-reader"
    .......
    (:DOUBAN-READER)
    
  ;切换package
  
    * (in-package :douban-reader)

    #<PACKAGE "DOUBAN-READER">
    
*2.接口使用*

以我的豆瓣uid（livevsevil)为例  
  
a.获取following  
  ;默认返回前100个
    
    *(fetch-following "livevsevil")
    
     (((:CITY . "上海")
       (:ICON--AVATAR . "http://img3.douban.com/icon/ui54577353-3.jpg")
       (:STATUSES--COUNT . 117) (:SCREEN--NAME . "Green") (:FOLLOWING--COUNT . 20)
       (:URL . "http://www.douban.com/people/54577353/")
       ...

  ;返回第101至150个

    (fetch-following "livevsevil" 100 50)
    
b.获取followers  
  ;默认返回前100个
  
    * (fetch-followers "livevsevil")

    (((:CITY . "台北")
      (:ICON--AVATAR . "http://img3.douban.com/icon/ui56969553-7.jpg")
      (:STATUSES--COUNT . 130) (:SCREEN--NAME . "Juanito") (:FOLLOWING--COUNT . 63)
      (:URL . "http://www.douban.com/people/56969553/")
      ...
     
  ;返回第101至150个
  
    * (fetch-followers "livevsevil" 100 50)
        
c.获取用户收藏的图书(默认返回前100个)  
  ;默认返回前100个
  
    * (fetch-book-collection "livevsevil")

    ((:COUNT . 100) (:START . 0) (:TOTAL . 174)
     (:COLLECTIONS
     ((:STATUS . "read") (:UPDATED . "2013-04-19 14:27:04")
      (:BOOK (:PUBLISHER . "The MIT Press") (:SUBTITLE . "")
      (:ISBN-10 . "0262560992") (:ISBN-13 . "9780262560993")
      (:TITLE . "The Little Schemer - 4th Edition")
      ...

  ;返回第101至150个

    *(fetch-book-collection "livevsevil" 100 50)
    
d.获取用户所有的following和followers

    * (get-people "livevsevil")

    #<HASH-TABLE :TEST EQUAL :COUNT 39 {B1BD001}>
    
e. 获取用户收藏的所有图书

    * (get-book-collection "livevsevil")

    ("The Little Schemer - 4th Edition"
     "The Scheme Programming Language, 4th Edition" "HTTP权威指南" "数据可视化之美"     ...

f.获取用户和别人收藏的相同的书籍
参数people为一个hash，以豆瓣显示的名字为key，uid为value。另外，豆瓣对访问次数有限制（API调用被限制为每分钟请求不超过10次。使用API Key时，对访问的限制较为宽松，为每分钟40次，超过限制的话会被封禁），如people的个数较多时，超过的获取不到数据。

    * (find-common-books "livevsevil" *people*)
    YGC:7 books in common.
    ...






