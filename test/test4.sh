echo "------------------------------"
echo "Unsuccesful tests"
echo "(3)Test of curl Tags"
TOKEN="localhost:8000/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiQWRtaW4iLCJ1c2VyX2lkIjoyOX0.EPnw0fsAP2uYpPHlBIxMBrg4GpZpjCd7vXkBcJPBoSc/1/"
TAGS="tags"
#--------------------------
TAGPOST="tags?text_of_tag=Ruby"
#--------------------------
echo -e "\n-----------(1.1)"
echo -e "GET"
curl $TOKEN$TAGS

echo -e "\n-----------(1.2)"
echo -e "POST"
curl -X POST $TOKEN$TAGPOST

echo -e "\n-----------(1.3)"
echo -e "GET"
curl $TOKEN$TAGS

echo -e "\n-----------(1.4)"
echo -e "PUT"
echo -e "Choose tag_id to PUT"
read tag_id 
echo -e "Choose text_of_tag to PUT"
read tag_name
TAGPUT="tags?tag_id="$tag_id"&text_of_tag="$tag_name
curl -X PUT $TOKEN$TAGPUT

echo -e "\n-----------(1.5)"
echo -e "GET"
curl $TOKEN$TAGS

echo -e "\n-----------(1.6)"
echo -e "DELETE"
echo -e "Choose tag_id to DELETE"
read catDel
TAGDELETE="tags?tag_id="$catDel
curl -X DELETE $TOKEN$TAGDELETE

echo -e "\n-----------(1.7)"
echo -e "GET"
curl $TOKEN$TAGS
