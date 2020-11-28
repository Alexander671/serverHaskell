
echo "------------------------------"
echo "Unsuccesful tests"
echo "(3)Test of curl Comments"
TOKEN="localhost:8000/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiQWRtaW4iLCJ1c2VyX2lkIjoyOX0.EPnw0fsAP2uYpPHlBIxMBrg4GpZpjCd7vXkBcJPBoSc/0/news/"
COMMENTS="7/comments"
#--------------------------
COMMENTPOST="7/comments?text_of_comment=I%20think%20this%20..."
#--------------------------
echo -e "\n-----------(1.1)"
echo -e "GET"
curl $TOKEN$COMMENTS
#-------------------------
echo -e "\n-----------(1.2)"
echo -e "POST"
curl -X POST $TOKEN$COMMENTPOST
#-------------------------
echo -e "\n-----------(1.3)"
echo -e "GET"
curl $TOKEN$COMMENTS
#-------------------------
echo -e "\n-----------(1.4)"
echo -e "DELETE"
echo -e "Choose comment_id to DELETE"
read comDel
COMMDELETE="7/comments?id_of_comment="$comDel
curl -X DELETE $TOKEN$COMMDELETE
#-------------------------
echo -e "\n-----------(1.5)"
echo -e "GET"
curl $TOKEN$COMMENTS
