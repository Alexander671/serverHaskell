# необходимо изменить работу routeDrafts
# расширить количество полей 
# здесь поменять токен
# а также изменить метод publish
echo "------------------------------"
echo "Unsuccesful tests"
echo "(3)Test of curl Drafts"
TOKEN="localhost:8000/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyX2lkIjoxMSwicm9sZSI6IkF1dG9yIn0.lsvxSz6fBcMUU4_CNfkxOMd4JmX63vkwQWdflned1Hw/0/"
DRAFTS="drafts"
#--------------------------
echo -e "\n-----------(1.1)"
echo -e "GET"
curl $TOKEN$DRAFTS

#-------------------------
echo -e "\n-----------(1.2)"
echo -e "POST"
echo "Choose name"
read name
echo "Choose text_of_drart"
read txt
echo "Choose category_id"
read cat
echo "Choose photo"
read pht
DRAFTPOST="drafts?name="$name"&category_id="$cat"&photo="$pht"&text_of_draft="$txt
curl -X POST $TOKEN$DRAFTPOST
#-------------------------
echo -e "\n-----------(1.3)"
echo -e "GET"
curl $TOKEN$DRAFTS
#-------------------------
echo -e "\n-----------(1.4)"
echo -e "PUT"
echo "Choose draft_id"
read id1
echo "Choose name"
read name1
echo "Choose text_of_drart"
read txt1
echo "Choose category_id"
read cat1
echo "Choose photo"
read pht1
echo "Choose tags [1,2,3..]"
read tgs1

DRAFTPUT="drafts?draft_id="$id1"&name="$name1"&text_of_draft="$txt1"&category_id="$cat1"&photo="$pht1"&tags="$tgs1
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

#------------------------
echo -e "\n-----------(1.7)"
echo -e "POST"
echo "Choose id_of_draft to publish"
read publ
DRAFTTOPUBL = "drafts/publish?id_of_draft="$publ
curl -X POST $TOKEN$publ
