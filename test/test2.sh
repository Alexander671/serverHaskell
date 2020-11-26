echo "------------------------------"
echo "Unsuccesful tests"
echo "(1)Test of curl Autors (GET METHOD)"
TOKEN="localhost:8000/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiQWRtaW4iLCJ1c2VyX2lkIjoyOX0.EPnw0fsAP2uYpPHlBIxMBrg4GpZpjCd7vXkBcJPBoSc/0/"
AUTORS="autors"
#--------------------------
AUTORSPOST="autors?user_id=12&description=ola-la"
#--------------------------
AUTORSPUT="autors?user_id_autors=12&description=ola"
#--------------------------
echo -e "\n-----------(1.1)"
echo -e "GET"
curl $TOKEN$AUTORS

echo -e "\n-----------(1.2)"
echo -e "POST"
curl -X POST $TOKEN$AUTORSPOST


echo -e "\n-----------(1.3)"
echo -e "GET"
curl $TOKEN$AUTORS


echo -e "\n-----------(1.4)"
echo -e "PUT"
AUTORSPUT="autors?description=ola-la&user_id=12"
curl -X PUT $TOKEN$AUTORSPUT

echo -e "\n-----------(1.5)"
echo -e "GET"
curl $TOKEN$AUTORS

echo -e "\n-----------(1.6)"
echo -e "DELETE"
AUTORSDELETE="autors?user_id=12"
curl -X DELETE $TOKEN$AUTORSDELETE

echo -e "\n-----------(1.7)"
echo -e "GET"
curl $TOKEN$AUTORS
