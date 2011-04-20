#ifndef SE_LISTVIEWDATASOURCE_H
#define SE_LISTVIEWDATASOURCE_H
class SE_ListViewDataSource
{
public:
    enum {OK, END_OF_DATASOURCE, WAIT_FOR_UPDATE};	
	virtual ~SE_ListDataSource() {}
	virtual int fillItem(SE_ListItem* listItem, unsigned int pos) = 0;
};
#endif
