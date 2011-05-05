#ifndef SE_LISTVIEWDATASOURCE_H
#define SE_LISTVIEWDATASOURCE_H
class SE_ListViewDataSource
{
public:
    enum {OK, END_OF_DATASOURCE, WAIT_FOR_UPDATE};	
	virtual ~SE_ListDataSource() {}
	virtual int fillItem(SE_ListItem* listItem, unsigned int pos) = 0;
};
struct SE_ListItemData
{
	SE_ImageUnit image1;
	SE_ImageUnit image2;
	SE_ImageUnit image3;
	SE_ImageUnit image4;
	SE_ImageUnit image5;
	SE_ImageUnit image6;
	SE_ImageUnit image7;
	SE_ImageUnit image8;
	std::string text1;
	std::string text2;
	std::string text3;
	std::string text4;
	std::string text4;
	std::string text5;
	std::string text6;
	std::string text7;
};
template <typename Data, typename ListItem>
class SE_ArrayDataSource : public SE_ListViewDataSource
{
public:
    //array can not be NULL	
	SE_ArrayDataSource(Data* array);
	int fillItem(SE_ListItem* listItem, unsigned int pos);
private:
    Data* mDataArray;	
};

class SE_StreamDataSource : public SE_ListViewDataSource
{
public:
    SE_StreamDataSource();
	int fillItem(SE_ListItem* listItem, unsigned int pos);
};

#endif
