package main

import (
	"fmt"
	"regexp"
)

func test(expression string, input []string){
	r, _ := regexp.Compile(expression)
	for i := 0; i < len(input); i++{
		cur := input[i]
		fmt.Println(r.MatchString(cur))
	}
}

func main() {
	// Email https://www.regexlib.com/REDetails.aspx?regexp_id=21
    test(
		"^[\\w-\\.]+@([\\w-]+\\.)+[\\w-]{2,4}$", 
		[]string{"joe@aol.com","joe@wrox.co.uk","joe@domain.info","a@b","notanemail","joe@@."},
	)

	// URL https://www.regexlib.com/REDetails.aspx?regexp_id=96
	test(
		"(http|ftp|https):\\/\\/[\\w\\-_]+(\\.[\\w\\-_]+)+([\\w\\-\\.,@?^=%&amp;:/~\\+#]*[\\w\\-\\@?^=%&amp;/~\\+#])?",
	     []string{"http://regxlib.com/Default.aspx","http://electronics.cnet.com/electronics/0-6342366-8-8994967-1.html","www.yahoo.com"},
	)

	// Singapore Phone/Mobile number
	// https://www.regexlib.com/Search.aspx?k=singapore&c=-1&m=-1&ps=20
	// https://www.regexlib.com/REDetails.aspx?regexp_id=3144
	test(
		"^[6]\\d{7}$|^[89]\\d{7}$",
		[]string{"61234567","63829324","67654321","6123-4567","6-CALL-CPY","6123abcd","81234532","92837465","8abec123"},
   )
}