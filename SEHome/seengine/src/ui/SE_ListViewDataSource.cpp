#include "SE_ListViewDataSource.h"
template <typename Data, typename ListItem>
SE_ArrayDataSource<Data, ListItem>::SE_ArrayDataSource(Data* array)
{
	mDataArray = Data;
}
template <typename Data, typename ListItem>
int SE_ArrayDataSource<Data, ListItem>::fillItem(SE_ListItem* listItem, unsigned int pos)
{
    if(pos >= mDataArray->size())
		return END_OF_DATASOURCE;
	Data* data = &(*mDataArray)[pos];
	listItem->fill(data);
	return OK;
}
