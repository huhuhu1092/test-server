#include "SE_ShaderProgram.h"
#include "SE_Log.h"
#include "SE_Common.h"
#include "SE_Mesh.h"
#include <string.h>
#include "SE_MemLeakDetector.h"
static void checkGLError()
{
    /*
    GLenum error = glGetError();
    if(error != GL_NO_ERROR)
    {
        LOGI("### gl error = %d ####\n", error);
        SE_ASSERT(0);
    }
    */
}
static GLuint loadShader(GLenum type, const char* shaderSrc)
{
    GLuint shader;
    GLint compiled;
    /*const char* shaderString[] = {shaderSrc, 0};*/
    LOGI("#### start load shader ##\n");
    shader = glCreateShader(type);
    if(shader == 0)
        return 0;

    glShaderSource(shader, 1, &shaderSrc, 0);
    checkGLError();
    glCompileShader(shader);
    LOGI("### compile ok ####\n");
    checkGLError();
    glGetShaderiv(shader, GL_COMPILE_STATUS, &compiled);
    checkGLError();
    if(!compiled)
    {
        GLint infoLen = 0;
        glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &infoLen);
        checkGLError();
        if(infoLen > 1)
        {
            char* infoLog = new char[sizeof(char) * infoLen];
            glGetShaderInfoLog(shader, infoLen, 0, infoLog);
            checkGLError();
            LOGI("Error compiling shader: \n%s\n", infoLog);
            delete[] infoLog;
        }
        glDeleteShader(shader);
        checkGLError();
        return 0;
    }
    return shader;
}
IMPLEMENT_OBJECT(SE_ShaderProgram)
SE_ShaderProgram::SE_ShaderProgram()
{
    mShaderProgramObject = -1;
    mHasInit = false;
    m_a_position_loc = -1;
    m_u_wvp_matrix_loc = -1;
    mVertexShaderSrc = NULL;
    mFragmentShaderSrc = NULL;
}
SE_ShaderProgram::SE_ShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc) : mHasInit(false) , mVertexShaderSrc(NULL), mFragmentShaderSrc(NULL)
{
#ifdef GLES_20
    LOGI("### construct shader program ###");

    init(vertexShaderSrc, fragmentShaderSrc);
    setSource(vertexShaderSrc, fragmentShaderSrc);
#endif
}
void SE_ShaderProgram::setSource(char* vertexShaderSrc, char* fragmentShaderSrc)
{
    if(mVertexShaderSrc)
        delete[] mVertexShaderSrc;
    if(mFragmentShaderSrc)
        delete[] mFragmentShaderSrc;
    int vertexShaderSrcLen = strlen(vertexShaderSrc);
    int fragmentShaderSrcLen = strlen(fragmentShaderSrc);
    mVertexShaderSrc = new char[vertexShaderSrcLen + 1];
    mFragmentShaderSrc = new char[fragmentShaderSrcLen + 1];
    strncpy(mVertexShaderSrc, vertexShaderSrc, vertexShaderSrcLen);
    strncpy(mFragmentShaderSrc, fragmentShaderSrc, fragmentShaderSrcLen);
    mVertexShaderSrc[vertexShaderSrcLen] = '\0';
    mFragmentShaderSrc[fragmentShaderSrcLen] = '\0';
    
}
void SE_ShaderProgram::create(char* vertexShaderSrc, char* fragmentShaderSrc)
{
#ifdef GLES_20
    LOGI("### create shader program ###");
    init(vertexShaderSrc, fragmentShaderSrc);
    setSource(vertexShaderSrc, fragmentShaderSrc);
#endif
}
GLuint SE_ShaderProgram::getHandler()
{
    return mShaderProgramObject;
}
bool SE_ShaderProgram::initOK()
{
    return mHasInit;
}
void SE_ShaderProgram::releaseHardwareResource()
{
    if(mHasInit)
    {
        glDeleteProgram(mShaderProgramObject);
    }
    mShaderProgramObject = 0;
    mHasInit = false;
}
void SE_ShaderProgram::recreate()
{
    LOGI("### recreate shader #####\n");
    if(mVertexShaderSrc == NULL)
        return;
    if(mFragmentShaderSrc == NULL)
        return;
    init(mVertexShaderSrc, mFragmentShaderSrc);
}
void SE_ShaderProgram::init(char* vertexShaderSrc, char* fragmentShaderSrc)
{
#ifdef GLES_20
    GLuint vertexShader;
    GLuint fragmentShader;
    GLuint programObject;
    GLint linked;
    vertexShader = loadShader(GL_VERTEX_SHADER, vertexShaderSrc);
    if(vertexShader == 0)
    {
        mHasInit = false;
        return;
    }
    LOGI("### load vertex shader OK ###\n");
    fragmentShader = loadShader(GL_FRAGMENT_SHADER, fragmentShaderSrc);
    if(fragmentShader == 0)
    {
        mHasInit = false;
        return ;
    }
    LOGI("### load fragment shader OK ####\n");
    programObject = glCreateProgram();
    if(programObject == 0)
    {
        mHasInit = false;
        return ;
    }
    glAttachShader(programObject, vertexShader);
    checkGLError();
    glAttachShader(programObject, fragmentShader);
    checkGLError();
    glLinkProgram(programObject);
    checkGLError();
    glGetProgramiv(programObject, GL_LINK_STATUS, &linked);
    checkGLError();
    if(!linked)
    {
        GLint infoLen = 0;
        glGetProgramiv(programObject, GL_INFO_LOG_LENGTH, &infoLen);
        checkGLError();
        if(infoLen > 1)
        {
            char* infoLog = new char[sizeof(char) * infoLen];
            glGetProgramInfoLog(programObject, infoLen, 0, infoLog);
            checkGLError();
            LOGI("Error linking program: \n%s\n", infoLog);
            delete[] infoLog;
        }
        glDeleteProgram(programObject);
        checkGLError();
        mHasInit = false;
        return;
    }
    LOGI("### link shader OK ####\n");
    glDeleteShader(vertexShader);
    checkGLError();
    glDeleteShader(fragmentShader);
    checkGLError();
    mShaderProgramObject = programObject;
    mHasInit = true;
#endif
}
void SE_ShaderProgram::use()
{
#ifdef GLES_20
    if(!mHasInit)
        return;
    link();
    glUseProgram(mShaderProgramObject);
    checkGLError();
#endif
}
SE_ShaderProgram::~SE_ShaderProgram()
{
    if(mHasInit)
    {
        glDeleteProgram(mShaderProgramObject);
    }
    if(mVertexShaderSrc)
        delete[] mVertexShaderSrc;
    if(mFragmentShaderSrc)
        delete[] mFragmentShaderSrc;
}
void SE_ShaderProgram::link()
{
#ifdef GLES_20
    m_a_position_loc = glGetAttribLocation(mShaderProgramObject, "a_position");
    m_u_wvp_matrix_loc = glGetUniformLocation(mShaderProgramObject, "u_wvp_matrix");
#ifdef DEBUG0
    LOGI("### m_a_position_loc = %d ###\n", m_a_position_loc);
    LOGI("### m_u_wvp_matrix_loc = %d ###\n", m_u_wvp_matrix_loc);
#endif
#endif
}
/////////////
IMPLEMENT_OBJECT(SE_SimpleSurfaceShaderProgram)
SE_SimpleSurfaceShaderProgram::SE_SimpleSurfaceShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc) : SE_ShaderProgram(vertexShaderSrc, fragmentShaderSrc)
{}
SE_SimpleSurfaceShaderProgram::~SE_SimpleSurfaceShaderProgram()
{}
SE_SimpleSurfaceShaderProgram::SE_SimpleSurfaceShaderProgram()
{
    m_a_tex_coord_loc = -1;
    m_u_texture_loc = -1;
    m_u_shading_mode_loc = -1; 
    m_u_color_loc = -1;
}
void SE_SimpleSurfaceShaderProgram::link()
{
#ifdef GLES_20
    SE_ShaderProgram::link();
    m_a_tex_coord_loc = glGetAttribLocation(mShaderProgramObject, "a_tex_coord");
    m_u_texture_loc = glGetUniformLocation(mShaderProgramObject, "u_texture");
    m_u_shading_mode_loc = glGetUniformLocation(mShaderProgramObject, "u_shading_mode");
    m_u_color_loc = glGetUniformLocation(mShaderProgramObject, "u_color");
    m_u_alpha_loc = glGetUniformLocation(mShaderProgramObject, "u_alpha");
#endif
}
/////////////
IMPLEMENT_OBJECT(SE_ColorExtractShaderProgram)
SE_ColorExtractShaderProgram::SE_ColorExtractShaderProgram()
{}
SE_ColorExtractShaderProgram::SE_ColorExtractShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc) : SE_ShaderProgram(vertexShaderSrc, fragmentShaderSrc)
{}
SE_ColorExtractShaderProgram::~SE_ColorExtractShaderProgram()
{}
void SE_ColorExtractShaderProgram::link()
{
#ifdef GLES_20
    SE_ShaderProgram::link();
    m_a_tex_coord0_loc = glGetAttribLocation(mShaderProgramObject, "a_tex_coord0");
    m_a_tex_coord1_loc = glGetAttribLocation(mShaderProgramObject, "a_tex_coord1");
    m_a_tex_coord2_loc = glGetAttribLocation(mShaderProgramObject, "a_tex_coord2");
    m_a_tex_coord3_loc = glGetAttribLocation(mShaderProgramObject, "a_tex_coord3");
    m_u_texture0_loc = glGetUniformLocation(mShaderProgramObject, "u_texture0");
    m_u_texture1_loc = glGetUniformLocation(mShaderProgramObject, "u_texture1");
    m_u_texture2_loc = glGetUniformLocation(mShaderProgramObject, "u_texture2");
    m_u_texture3_loc = glGetUniformLocation(mShaderProgramObject, "u_texture3");
    m_u_tex0_coord_index_loc = glGetUniformLocation(mShaderProgramObject, "u_tex0_coord_index");
    m_u_tex1_coord_index_loc = glGetUniformLocation(mShaderProgramObject, "u_tex1_coord_index");
    m_u_tex2_coord_index_loc = glGetUniformLocation(mShaderProgramObject, "u_tex2_coord_index");
    m_u_tex3_coord_index_loc = glGetUniformLocation(mShaderProgramObject, "u_tex3_coord_index");
    m_u_tex_combine_mode_loc = glGetUniformLocation(mShaderProgramObject, "u_tex_combine_mode");
    m_u_color_op_mode_loc = glGetUniformLocation(mShaderProgramObject, "u_color_op_mode");
    m_u_colora_loc = glGetUniformLocation(mShaderProgramObject, "u_colora");
    m_u_colorr_loc = glGetUniformLocation(mShaderProgramObject, "u_colorr");
    m_u_colorg_loc = glGetUniformLocation(mShaderProgramObject, "u_colorg");
    m_u_colorb_loc = glGetUniformLocation(mShaderProgramObject, "u_colorb");
    m_u_rchannelindex_loc = glGetUniformLocation(mShaderProgramObject, "u_rchannelindex");
    m_u_gchannelindex_loc = glGetUniformLocation(mShaderProgramObject, "u_gchannelindex");
    m_u_bchannelindex_loc = glGetUniformLocation(mShaderProgramObject, "u_bchannelindex");
    m_u_achannelindex_loc = glGetUniformLocation(mShaderProgramObject, "u_achannelindex");
#endif
}
GLint SE_ColorExtractShaderProgram::getTextureCoordAttributeLoc(int index)
{
    GLint ret = -1;
    switch(index)
    {
    case 0:
        ret = m_a_tex_coord0_loc;
        break;
    case 1:
        ret = m_a_tex_coord1_loc;
        break;
    case 2:
        ret = m_a_tex_coord2_loc;
        break;
    case 3:
        ret = m_a_tex_coord3_loc;
        break;
    default:
        break;
    }
    return ret;
}
GLint SE_ColorExtractShaderProgram::getTextureUniformLoc(int index)
{
    GLint ret = -1;
    switch(index)
    {
    case 0:
        ret = m_u_texture0_loc;
        break;
    case 1:
        ret = m_u_texture1_loc;
        break;
    case 2:
        ret = m_u_texture2_loc;
        break;
    case 3:
        ret = m_u_texture3_loc;
        break;
    default:
        break;
    }
    return ret;
}
GLint SE_ColorExtractShaderProgram::getTexCoordIndexUniformLoc(int index)
{
    GLint ret = -1;
    switch(index)
    {
    case 0:
        ret = m_u_tex0_coord_index_loc;
        break;
    case 1:
        ret = m_u_tex1_coord_index_loc;
        break;
    case 2:
        ret = m_u_tex2_coord_index_loc;
        break;
    case 3:
        ret = m_u_tex3_coord_index_loc;
        break;
    }
    return ret;
}
GLint SE_ColorExtractShaderProgram::getMarkColorUniformLoc(int index)
{
    GLint ret = -1;
    switch(index)
    {
    case 0:
        ret = m_u_colora_loc;
        break;
    case 1:
        ret = m_u_colorr_loc;
        break;
    case 2:
        ret = m_u_colorg_loc;
        break;
    case 3:
        ret = m_u_colorb_loc;
        break;
    }
    return ret;
}
GLint SE_ColorExtractShaderProgram::getColorChannelIndexUniformLoc(int index)
{
    GLint ret = -1;
    switch(index)
    {
    case 0:
        ret = m_u_rchannelindex_loc;
        break;
    case 1:
        ret = m_u_gchannelindex_loc;
        break;
    case 2:
        ret = m_u_bchannelindex_loc;
        break;
    case 3:
        ret = m_u_achannelindex_loc;
        break;
    }
    return ret;
}
/////////////////////////////////////////////
IMPLEMENT_OBJECT(SE_ColorEffectShaderProgram)
SE_ColorEffectShaderProgram::SE_ColorEffectShaderProgram()
{
    m_a_tex_coord0 = -1;
    m_u_texture_background = -1;
    m_u_texture_channel = -1;
    m_u_texture_texr = -1;
    m_u_texture_texg = -1;
    m_u_texture_texb = -1;
    m_u_texture_texa = -1;
    m_u_has_texr = -1;
    m_u_has_texg = -1;
    m_u_has_texb = -1;
    m_u_has_texa = -1;
    m_u_markr_alpha = -1;
    m_u_markg_alpha = -1;
    m_u_markb_alpha = -1;
    m_u_marka_alpha = -1;
    m_u_background_alpha = -1;
    m_u_markr_fn = -1;
    m_u_markg_fn = -1;
    m_u_markb_fn = -1;
    m_u_marka_fn = -1;
    m_u_colorr = -1;
    m_u_colorg = -1;
    m_u_colorb = -1;
    m_u_colora = -1;
}
void SE_ColorEffectShaderProgram::link()
{
#ifdef GLES_20
    SE_ShaderProgram::link();
    m_a_tex_coord0 = glGetAttribLocation(mShaderProgramObject, "a_tex_coord0");
    m_u_texture_background = glGetUniformLocation(mShaderProgramObject, "u_texture_background");
    m_u_texture_channel = glGetUniformLocation(mShaderProgramObject, "u_texture_channel");
    m_u_texture_texr = glGetUniformLocation(mShaderProgramObject, "u_texture_texr");
    m_u_texture_texg = glGetUniformLocation(mShaderProgramObject, "u_texture_texg");
    m_u_texture_texb = glGetUniformLocation(mShaderProgramObject, "u_texture_texb");
    m_u_texture_texa = glGetUniformLocation(mShaderProgramObject, "u_texture_texa");
    m_u_has_texr = glGetUniformLocation(mShaderProgramObject, "u_has_texr");
    m_u_has_texg = glGetUniformLocation(mShaderProgramObject, "u_has_texg");
    m_u_has_texb = glGetUniformLocation(mShaderProgramObject, "u_has_texb");
    m_u_has_texa = glGetUniformLocation(mShaderProgramObject, "u_has_texa");
    m_u_markr_alpha = glGetUniformLocation(mShaderProgramObject, "u_markr_alpha");
    m_u_markg_alpha = glGetUniformLocation(mShaderProgramObject, "u_markg_alpha");
    m_u_markb_alpha = glGetUniformLocation(mShaderProgramObject, "u_markb_alpha");
    m_u_marka_alpha = glGetUniformLocation(mShaderProgramObject, "u_marka_alpha");
    m_u_background_alpha = glGetUniformLocation(mShaderProgramObject, "u_background_alpha");
    m_u_markr_fn = glGetUniformLocation(mShaderProgramObject, "u_markr_fn");
    m_u_markg_fn = glGetUniformLocation(mShaderProgramObject, "u_markg_fn");
    m_u_markb_fn = glGetUniformLocation(mShaderProgramObject, "u_markb_fn");
    m_u_marka_fn = glGetUniformLocation(mShaderProgramObject, "u_marka_fn");
    m_u_colorr = glGetUniformLocation(mShaderProgramObject, "u_colorr");
    m_u_colorg = glGetUniformLocation(mShaderProgramObject, "u_colorg");
    m_u_colorb = glGetUniformLocation(mShaderProgramObject, "u_colorb");
    m_u_colora = glGetUniformLocation(mShaderProgramObject, "u_colora");
#endif
}

////////////////////

IMPLEMENT_OBJECT(SE_FadeInOutShaderProgram)
SE_FadeInOutShaderProgram::SE_FadeInOutShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc) : SE_ShaderProgram(vertexShaderSrc, fragmentShaderSrc)
{
    m_a_tex_coord_loc = -1;
    m_u_texture_loc = -1;
    m_u_shading_mode_loc = -1; 
    m_u_color_loc = -1;
}
SE_FadeInOutShaderProgram::~SE_FadeInOutShaderProgram()
{}
SE_FadeInOutShaderProgram::SE_FadeInOutShaderProgram()
{    
}
void SE_FadeInOutShaderProgram::link()
{
#ifdef GLES_20
    SE_ShaderProgram::link();
    m_a_tex_coord_loc = glGetAttribLocation(mShaderProgramObject, "a_tex_coord");
    m_u_texture_loc = glGetUniformLocation(mShaderProgramObject, "u_texture");
    m_u_shading_mode_loc = glGetUniformLocation(mShaderProgramObject, "u_shading_mode");
    m_u_color_loc = glGetUniformLocation(mShaderProgramObject, "u_color");
    m_u_alpha_loc = glGetUniformLocation(mShaderProgramObject, "u_alpha");    
#endif
}
////////////////////////////////////

/////////////
IMPLEMENT_OBJECT(SE_SkeletalAnimationShaderProgram)
SE_SkeletalAnimationShaderProgram::SE_SkeletalAnimationShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc) : SE_ShaderProgram(vertexShaderSrc, fragmentShaderSrc)
{
    m_a_tex_coord_loc = -1;
    m_u_texture_loc = -1;
    m_u_shading_mode_loc = -1; 
    m_u_color_loc = -1;
}
SE_SkeletalAnimationShaderProgram::~SE_SkeletalAnimationShaderProgram()
{}
SE_SkeletalAnimationShaderProgram::SE_SkeletalAnimationShaderProgram()
{    
}
void SE_SkeletalAnimationShaderProgram::link()
{
#ifdef GLES_20
    SE_ShaderProgram::link();
    m_a_tex_coord_loc = glGetAttribLocation(mShaderProgramObject, "a_tex_coord");
    m_u_texture_loc = glGetUniformLocation(mShaderProgramObject, "u_texture");
    m_u_shading_mode_loc = glGetUniformLocation(mShaderProgramObject, "u_shading_mode");
    m_u_color_loc = glGetUniformLocation(mShaderProgramObject, "u_color");
    m_u_alpha_loc = glGetUniformLocation(mShaderProgramObject, "u_alpha");


    //debug code
    m_u_boneindex_loc = glGetAttribLocation(mShaderProgramObject, "boneIndex");
    m_u_boneweight_loc = glGetAttribLocation(mShaderProgramObject, "boneWeight");
    m_u_bonematrix_loc =glGetUniformLocation(mShaderProgramObject, "boneMatricesPerFrame");
#endif
}
/////////////

////////////////////

IMPLEMENT_OBJECT(SE_SimpleLightingShaderProgram)
SE_SimpleLightingShaderProgram::SE_SimpleLightingShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc) : SE_ShaderProgram(vertexShaderSrc, fragmentShaderSrc)
{
    m_a_tex_coord_loc = -1;
    m_u_texture_loc = -1;
    m_u_shading_mode_loc = -1; 
    m_u_color_loc = -1;
    m_u_lightPos_loc = -1;
    m_u_eyePos_loc = -1;
    u_m2w_matrix_loc = -1;

    m_u_ambient_loc = -1;
    m_u_diffuse_loc = -1;
    m_u_specular_loc = -1;
    m_u_shiness_loc = -1;
}
SE_SimpleLightingShaderProgram::~SE_SimpleLightingShaderProgram()
{}
SE_SimpleLightingShaderProgram::SE_SimpleLightingShaderProgram()
{    
}
void SE_SimpleLightingShaderProgram::link()
{
#ifdef GLES_20
    SE_ShaderProgram::link();
    m_a_tex_coord_loc = glGetAttribLocation(mShaderProgramObject, "a_tex_coord");
    m_u_texture_loc = glGetUniformLocation(mShaderProgramObject, "u_texture");
    m_u_shading_mode_loc = glGetUniformLocation(mShaderProgramObject, "u_shading_mode");
    m_u_color_loc = glGetUniformLocation(mShaderProgramObject, "u_color");
    m_u_lightPos_loc = glGetUniformLocation(mShaderProgramObject, "u_lightPos");
    m_u_eyePos_loc = glGetUniformLocation(mShaderProgramObject, "u_eyePos"); 
    u_m2w_matrix_loc = glGetUniformLocation(mShaderProgramObject, "u_m2w_matrix"); 

    m_u_ambient_loc = glGetUniformLocation(mShaderProgramObject, "u_ambient");
    m_u_diffuse_loc = glGetUniformLocation(mShaderProgramObject, "u_diffuse");
    m_u_specular_loc = glGetUniformLocation(mShaderProgramObject, "u_specular");
    m_u_shiness_loc = glGetUniformLocation(mShaderProgramObject, "u_shiness");
#endif
}
////////////////////////////////////

////////////////////

IMPLEMENT_OBJECT(SE_NormalMapShaderProgram)
SE_NormalMapShaderProgram::SE_NormalMapShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc) : SE_SimpleLightingShaderProgram(vertexShaderSrc, fragmentShaderSrc)
{
    m_u_vertex_tangent_loc = -1;
    m_u_normal_map_loc = -1;
}
SE_NormalMapShaderProgram::~SE_NormalMapShaderProgram()
{}
SE_NormalMapShaderProgram::SE_NormalMapShaderProgram()
{    
}
void SE_NormalMapShaderProgram::link()
{
#ifdef GLES_20
    SE_SimpleLightingShaderProgram::link();    

    //Normal map
    m_u_vertex_tangent_loc = glGetAttribLocation(mShaderProgramObject, "a_vert_tangent");
    m_u_normal_map_loc = glGetUniformLocation(mShaderProgramObject, "u_normal_map");
    
#endif
}
////////////////////////////////////