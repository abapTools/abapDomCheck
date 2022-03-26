![Software License](https://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat-square)
![ABAP Version](https://img.shields.io/badge/ABAP%20-7.51-brightgreen)


## abapDomCheck
SAP only check data against the domain fixed values and check tables in the GUI. This causes problems during data import, for example, as these checks do not take place there. This could result in incorrect data entering the system. This project was created for this purpose.



```abap
data type ref to zcl_dom_check.
lo_object = zcl_dom_check=>get_instance( ).

data(rejected) = lo_object->check_fix_values_struc( ls_dataset ).

if rejected eq abap_true.
	data(lt_msg) = lo_object->get_msg().
endif.

```
