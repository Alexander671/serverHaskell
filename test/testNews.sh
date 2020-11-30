echo "------------------------------"
echo "Unsuccesful tests"
echo "(1)Test of curl News (GET METHOD)"
TOKEN="http://localhost:8000/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiVXNlciIsInVzZXJfaWQiOjI5fQ.6NN9o0rSpA9wC9wo7oJnJNI_WTPuBfXLl5aU7gmcZrA/0/"
NEWS="news"
NEWSFILTER1="news/filter?category=1"
NEWSFILTER2="news/filter?category=6"
NEWSFILTER3="news/filter?category=7"
NEWSFILTER4="news/filter?date_of_create_at_gt=25-05-2005"
NEWSFILTER5="news/filter?date_of_create_at_lt=25-05-2025"
NEWSFILTER6="news/filter?date_of_create_at_gt=25-05-2025"
NEWSFILTER7="news/filter?date_of_create_at_lt=25-05-2005"
NEWSFILTER8="news/filter?date_of_create_at_gt=01-09-2020"
NEWSFILTER9="news/filter?name_of_autor=Ivan"
NEWSFILTER10="news/filter?name_of_autor=Petr"
#--------------------------
#--------------------------
NEWSORDER1="news/order?category=ASC"
NEWSORDER2="news/order?category=DESC"
NEWSORDER3="news/order?category=ASC"     
NEWSORDER4="news/order?date_of_create=ASC"
NEWSORDER5="news/order?date_of_create=DESC"
NEWSORDER6="news/order?name_of_autor=ASC"
NEWSORDER7="news/order?name_of_autor=DESC"

NEWSSHORT="news/short"
NEWSID="news/filter?id_of_new=1"

NEWSEARCH="news/search?content=а"
curl $TOKEN$NEWS
echo -e "\n-----------(1.1)"
curl $TOKEN$NEWSFILTER1
echo -e "\n-----------(1.2)"
curl $TOKEN$NEWSFILTER2
echo -e "\n-----------(1.3)"
curl $TOKEN$NEWSFILTER3
echo -e "\n-----------(1.4)"
curl $TOKEN$NEWSFILTER4
echo -e "\n-----------(1.5)"
curl $TOKEN$NEWSFILTER5
echo -e "\n-----------(1.6)"
curl $TOKEN$NEWSFILTER6
echo -e "\n-----------(1.7)"
curl $TOKEN$NEWSFILTER7
echo -e "\n-----------(1.8)"
curl $TOKEN$NEWSFILTER8
echo -e "\n-----------(1.9)"
curl $TOKEN$NEWSFILTER9
echo -e "\n-----------(1.10)"
curl $TOKEN$NEWSFILTER10

echo -e "\n(2)Test of curl News (GET METHOD)"
echo -e "\n-----------(2.1)"
curl $TOKEN$NEWSORDER1
echo -e "\n-----------(2.2)"
curl $TOKEN$NEWSORDER2
echo -e "\n-----------(2.3)"
curl $TOKEN$NEWSORDER3
echo -e "\n-----------(2.4)"
curl $TOKEN$NEWSORDER4
echo -e "\n-----------(2.5)"
curl $TOKEN$NEWSORDER5
echo -e "\n-----------(2.6)"
curl $TOKEN$NEWSORDER6
echo -e "\n-----------(2.7)"
curl $TOKEN$NEWSORDER7

echo -e "\n-----------(2.8)"
curl $TOKEN$NEWSEARCH



