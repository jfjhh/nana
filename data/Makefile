DISK:=bochs.img
D=../$(DISK)
LIST=data.txt
WRITE_DATA=../write_data
UPDATE_DATA=update_data.sh

all: $(UPDATE_DATA)

$(UPDATE_DATA): $(LIST) $(WRITE_DATA) $(D)
	@./$@ $^

.PHONY: $(UPDATE_DATA)

