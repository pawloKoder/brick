RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

if [ ! -f interpreter ];
then
   echo -e "${RED}FAIL${NC} : interpreter does not exist"
   exit
fi

for i in ./test/auto/* ; do
        echo -n "Test: "$i
        ./interpreter $i
        if [[ $? -eq 0 ]] ; then
                echo -e ": ${GREEN}OK${NC}"
        else
                echo -e ": ${RED}FAIL${NC}"
        fi
done

