# необходимо изменить работу routeDrafts
# расширить количество полей 
# здесь поменять токен
# а также изменить метод publish
echo "------------------------------"
echo "Unsuccesful tests"
echo "(3)Test of curl Drafts"
TOKEN="localhost:8000/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiQXV0b3IiLCJ1c2VyX2lkIjoxMX0.78929xycePxnTShcdPVzkvT4tSnV11-Uc5Q1eLOOfc0/0/"
DRAFTS="drafts"
#--------------------------
echo -e "\n-----------(1.1)"
echo -e "GET"
curl $TOKEN$DRAFTS

#-------------------------
echo -e "\n-----------(1.2)"
echo -e "POST"
echo "Choose text_of_drart"
read txt
echo "Choose category_id"
read cat
echo "Choose photo"
read pht
DRAFTPOST="drafts?text_of_draft="$txt"&category_id="$cat"&photo="$pht
curl -X POST $TOKEN$DRAFTPOST
#-------------------------
echo -e "\n-----------(1.3)"
echo -e "GET"
curl $TOKEN$DRAFTS
#-------------------------
echo -e "\n-----------(1.4)"
echo -e "PUT{"
echo "Choose draft_id"
read id
echo "Choose text_of_drart"
read txt
echo "Choose category_id"
read cat
echo "Choose photo"
read pht
DRAFTPUT="drafts?draft_id="$id"&text_of_draft="$txt"&category_id="$cat"&photo="$pht
curl -X PUT $TOKEN$DRAFTPUT
#-------------------------
echo -e "\n-----------(1.5)"
echo -e "GET"
curl $TOKEN$DRAFTS
#-------------------------
echo -e "\n-----------(1.6)"
echo -e "DELETE"
echo -e "Choose draft_id to DELETE"
read draftDel
DRAFTDEL="drafts?id_of_draft="$draftDel
curl -X DELETE $TOKEN$DRAFTDEL
#-------------------------
echo -e "\n-----------(1.7)"
echo -e "GET"
curl $TOKEN$DRAFTS
