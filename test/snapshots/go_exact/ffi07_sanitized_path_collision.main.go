package main

import (
	mext_example_x2e_com_acme_text_x2d_case "example.com/acme/text-case"
	mext_example_x2e_com_acme_text_u_case "example.com/acme/text_case"
)

func extern__example_x2e_com_acme_text_x2d_case__F(s string) string {
	return mext_example_x2e_com_acme_text_x2d_case.F(s)
}

func extern__example_x2e_com_acme_text_u_case__F(s string) string {
	return mext_example_x2e_com_acme_text_u_case.F(s)
}


func main() {
    _ = extern__example_x2e_com_acme_text_x2d_case__F("a")
    _ = extern__example_x2e_com_acme_text_u_case__F("b")
}
