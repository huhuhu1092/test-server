#ifndef SE_LIST_H
#define SE_LIST_H
template<class T>
class SE_List
{
public:
    struct SE_List_Node
    {
        T value;
        SE_ListNode* prev;
        SE_ListNode* next;
    };
    SE_List();
    ~SE_List();

private:
    SE_ListNode* head, tail;
};
#endif
