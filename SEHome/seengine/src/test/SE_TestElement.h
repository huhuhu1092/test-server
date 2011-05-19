class SE_Element
{
public:
    enum STATE {NORMAL, SELECTED, HIGHLIGHTED, INVISIBLE, INACTIVE, 
                ANIMATE_BEGIN, ANIMATE_RUNNING, ANIMATE_SUSPEND, ANIMATE_END};
    SE_Spatial* createSpatial();
    void update();
    void spawn();
    void measure();

private:
    int mState;
};
