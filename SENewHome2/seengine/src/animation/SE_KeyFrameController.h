#ifndef SE_KEYFRAMECONTROLLER_H
#define SE_KEYFRAMECONTROLLER_H
class SE_KeyFrameController
{
public:
    SE_KeyFrameController();
private:
    int mKeyFrameNum;
    SE_KeyFrame* mKeyFrames;
    std::string mName;
    std::string mParent;
};
#endif
