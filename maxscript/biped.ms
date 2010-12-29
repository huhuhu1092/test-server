/*
    name <! string 
    children <! array of seBipedNode
    skeleton <! seSkeleton
*/
struct seBipedNode (name, children = #(), skeleton)
/*
    rotation <! Quaternion : local rotation
    translate <! vector3 : local translate
    scale <! vector3 : local translate
*/
struct seJointPose (rotation, translate, scale)
/*
   time <! float : key time
   jointpos <! seJointPose
*/
struct seKeyFrame (time, jointpos)
/*
   bindpose <! matrix4 : inverse of bind pose matrix
   keyframes <! array of seKeyFrame 
*/
struct seSkeleton (bindpose, keyframes = #())

global seRootBipeNode = #() -- root bipe will not be one
seSceneChildren = rootNode.children
/*
   parent is seBipedNode and child is type Node
*/
fn seSetBipedNode parent child = 
(
    local bipNode = undefined
    if classOf child as string == "Biped_Object"
    then
    (
        if parent == undefined 
        then
        (
            bipNode = seBipeNode()
            bipNode.name = child.name
            append seRootBipeNode bipNode
        )
        else
        (
            bipNode = seBipeNode()
            bipNode.name = childName
            append parent.children child
        )
        local seChildren = child.children
        for i = 1 to seChildren.count do
        (
            seSetBipedNode bipNode seChildren[i]
        )
    )
)
/*
bipedNode <! seBipedNode
*/
fn seSetKeyFrame bipedNode = 
(
    -- get the node of bipedNode.name
    bn = getNodeByName bipedNode.name
    bipController = bn.transform.controller
    select bn
    lSkeleton = seSkeleton()
    lSkeleton.bindpose = bn.transform
    animate on
    (
        for i = 1 to bipController.keys.count do
        (
            key = biped.getKey bipController i
            local pos = undefined, scale = undefined, rotation = undefined
            at time key.time (pos = biped.getTransform bn #pos ; scale = biped.getTransform bn #scele ; rotation = biped.getTransform bn #rotation)
            lkeyframe = seKeyFrame()
            lkeyframe.time = key.time
            lkeyframe.jointpose = seJointPose()
            lkeyframe.jointpose.rotation = rotation
            lkeyframe.jointpose.translate = pos
            lkeyframe.jointpose.scale = scale
            append lSkeleton.keyframes lkeyframe
        )
    )
)
fn seSetKeyFrameToBipe bipedRoot =
(
    seSetKeyFrame bipedRoot
    for i = 1 to bipedRoot.children.count
    (
        child = bipedRoot.children[i]
        seSetKeyFrame child
    )
)
fn outputBipeNode outstream bipedNode =
(
    format "            *NODE : " to:outstream
    format "% : " bipedNode.name to:outstream
    for i = 1 to bipedNode.children.count do
    (
        child = bipedNode.children[i]
        format "% " child.name to:outstream
    )
    format "\n" to:outstream
    for i = 1 to bipedNode.children.count do
    (
        child = bipedNode.children[i]
        outputBipeNode outstream child
    )   
) 
fn outputBipeTransform outstream bipedNode = 
(
    format "            *TRANSFORM {\n" to:outstream
    format "                *NODE_NAME : %\n" bipedNode.name to:outstream
    format "                *BIND_POS : %\n" bipedNode.skeleton.bindpos to:outstream
    format "                *KEY_FRAME {\n" to:outstream
    for i = 1 to bipedNode.skeleton.keyframes.count do
    (
        local keyframe = bipedNode.skeleton.keyframes[i]
        format "                *FRAME % : " keyframe.time to:outstream
        format "% % %\n" keyframe.jointpos.rotation keyframe.jointpos.translate keyframe.jointpos.scale to:outstream
    )
    format "\n                }\n"
    format "\n            }\n" to:outstream
    for i = 1 to bipedNode.children.count do
    (
        child = bipedNode.children[i]
        outputBipeTransform outstream child
    )
)
fn outputBipeTransformInfo outstream bipedRoot = 
(
    format "        *TRANSFORM_INFO {\n" to:outstream
    outputBipeTransform outstream bipedRoot
    format "\n        }\n" to:outstream
)
fn outputBipeNodeInfo outstream bipedRoot = 
(
    format "        *NODE_INFO {\n" to:outstream
    outputBipeNode outstream bipedRoot
    format "\n        }\n" to:outstream
)
fn outputSkeletonInfomation outstream bipedRoot = 
(
    format "    *SKELETON {\n" to:outstream
    outputBipeNodeInfo outstream bipedRoot
    outputBipeTransformInfo outstream bipedRoot
    format "\n    }\n" to:outstream
)
for i = 1 to seSceneChildren.count do
(
    local seChild = seSceneChildren[i]
    if(classOf seChild as string == "Biped_Object")
    then
        seSetBipedNode undefined seChild
)
for i = 1 to seRootBipeNode.count do
(
    child = seRootBipeNode[i]
    seSetKeyFrameToBipe child
)
fs = createFile "C:\\model\\newhome3\\skeletoninfo.ASE"
format "*SKELETON_INFO {\n" to:fs
format "    *NUM %\n" seRootBipeNode.count to:fs

for i = 1 to seRootBipeNode.count do
(
    child = seRootBipeNode[i]
    outputSkeletonInformation fs child
)
format "\n}"
/*
bipNode = getNodeByName "Bip01 Head"
bip = bipNode.transform.controller
bip.keys[2]
objNode = getNodeByName "Object14"
objNode.modifiers
objNode.modifiers.count
objModifier = objNode.modifiers[1]
-- modPanel.setCurrentObject objModifier
boneNum = skinops.getNumberBones objModifier
select bipNode
for i = 1 to bip.keys.count do
(
	key = biped.getKey bip i
	format "%\n" key.time
)
animate on
(
	for i = 1 to bip.keys.count do
	(
		key = biped.getKey bip i
		at time key.time rot = biped.getTransform bipNode #scale
		format "%\n" rot
	)
)
*/
--select objNode
/*
for i = 1 to boneNum do
(
    boneName = skinops.getBoneName objModifier i 1
	format "%\n" boneName
)
*/
-- boneName = skinops.getBoneName objModifier 1 1
