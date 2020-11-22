echo "------------------------------"
echo "Unsuccesful tests"
echo "(3)Test of curl Category"
TOKEN="localhost:8000/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiQWRtaW4iLCJ1c2VyX2lkIjoyOX0.EPnw0fsAP2uYpPHlBIxMBrg4GpZpjCd7vXkBcJPBoSc/1/"
CATEGORIES="categories"
#--------------------------
CATEGORIESPOST="categories?parent_id=12&category_name=ola-la"
#--------------------------
CATEGORIESDELETE="categories?category_id=16"
#--------------------------
CATEGORIESPUT="categories?category_id=16&category_name=ola&parent_id=11"
#--------------------------
echo -e "\n-----------(1.1)"
echo -e "GET"
curl $TOKEN$CATEGORIES

echo -e "\n-----------(1.2)"
echo -e "POST"
curl -X POST $TOKEN$CATEGORIESPOST

echo -e "\n-----------(1.3)"
echo -e "GET"
curl $TOKEN$CATEGORIES

echo -e "\n-----------(1.4)"
echo -e "PUT"
curl -X PUT $TOKEN$CATEGORIESPUT

echo -e "\n-----------(1.5)"
echo -e "GET"
curl $TOKEN$CATEGORIES

echo -e "\n-----------(1.6)"
echo -e "DELETE"
curl -X DELETE $TOKEN$CATEGORIESDELETE

echo -e "\n-----------(1.7)"
echo -e "GET"
curl $TOKEN$CATEGORIES
