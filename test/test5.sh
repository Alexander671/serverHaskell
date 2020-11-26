echo "------------------------------"
echo "Unsuccesful tests"
echo "(3)Test of curl Users,users/me,registration"
TOKEN="localhost:8000/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiQWRtaW4iLCJ1c2VyX2lkIjoyOX0.EPnw0fsAP2uYpPHlBIxMBrg4GpZpjCd7vXkBcJPBoSc/1/"
USERS="users"
USERME="users/me"
#--------------------------
TOKEN1="localhost:8000/"
USERSPOST="registration?login=Oleg&password=Oleg"
#--------------------------
USERLOGIN="login?login=Oleg&password=Oleg"
#--------------------------
echo -e "\n-----------(1.1)"
echo -e "GET"
curl $TOKEN$USERS

#--------------------------
echo -e "\n-----------(1.2)"
echo -e "GET"
curl $TOKEN$USERME


# post запрос для users
# находится в отдельном роутинге registration
# при регистрации автоматически создаётся пользователь.

echo -e "\n-----------(1.3)"
echo -e "POST"
curl -X POST $TOKEN1$USERSPOST

# login
echo -e "\n-----------(1.4)"
echo -e "POST"
curl -X POST $TOKEN1$USERLOGIN


echo -e "\n-----------(1.5)"
echo -e "GET"
curl $TOKEN$USERS

echo -e "\n-----------(1.6)"
echo -e "PUT"
echo -e "Choose first_name to PUT"
read first_name
echo -e "Choose second_name to PUT"
read second_name
echo -e "Choose image to PUT"
read image
USERPUT="users?first_name="$first_name"&second_name="$second_name"&image="$image
curl -X PUT $TOKEN$USERPUT

echo -e "\n-----------(1.7)"
echo -e "GET"
curl $TOKEN$USERS

echo -e "\n-----------(1.8)"
echo -e "DELETE"
echo -e "Choose user_id to DELETE"
read userDel
USERDELETE="users?user_id="$userDel
curl -X DELETE $TOKEN$USERDELETE

echo -e "\n-----------(1.9)"
echo -e "GET"
curl $TOKEN$USERS
