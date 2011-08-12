#ifndef SE_SHADERPROGRAM_H
#define SE_SHADERPROGRAM_H
#ifdef GLES_20
#include <GLES2/gl2.h>
#endif
#include "SE_Object.h"
class SE_ShaderProgram : public SE_Object
{
    DECLARE_OBJECT(SE_ShaderProgram)
public:
    SE_ShaderProgram();
    SE_ShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc);
    virtual ~SE_ShaderProgram();
    virtual void releaseHardwareResource();
    void create(char* vertexShader, char* fragmentShader);
    void setSource(char* vertexShaderSrc, char* fragmentShaderSrc);
    GLuint getHandler();
    void recreate();
    bool initOK();
    void use();
    GLint getPositionAttributeLoc()
    {
        return m_a_position_loc;
    }
    GLint getWorldViewPerspectiveMatrixUniformLoc()
    {
        return m_u_wvp_matrix_loc;
    }
protected:
    virtual void link();
    void init(char* vertexShaderSrc, char* fragmentShaderSrc);
protected:
    GLuint mShaderProgramObject;
    bool mHasInit;
    GLint m_a_position_loc;
    GLint m_u_wvp_matrix_loc;
    char* mVertexShaderSrc;
    char* mFragmentShaderSrc;
};
class SE_SimpleSurfaceShaderProgram : public SE_ShaderProgram
{
    DECLARE_OBJECT(SE_SimpleSurfaceShaderProgram)
public:
    SE_SimpleSurfaceShaderProgram();
    SE_SimpleSurfaceShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc);
    ~SE_SimpleSurfaceShaderProgram();
    GLint getTexCoordAttributeLoc()
    {
        return m_a_tex_coord_loc;
    }
    GLint getTextureUniformLoc()
    {
        return m_u_texture_loc;
    }
    GLint getColorUniformLoc()
    {
        return m_u_color_loc;
    }
    GLint getShadingModeUniformLoc()
    {
        return m_u_shading_mode_loc;
    }

    GLint getAlphaUniformLoc()
    {
        return m_u_alpha_loc;
    }
protected:
    virtual void link();
private:
    GLint m_u_color_loc;
    GLint m_a_tex_coord_loc;
    GLint m_u_texture_loc;
    GLint m_u_shading_mode_loc;
    //single alpha 
    GLint m_u_alpha_loc;
};
class SE_ColorExtractShaderProgram : public SE_ShaderProgram
{
    DECLARE_OBJECT(SE_ColorExtractShaderProgram)
public:
    SE_ColorExtractShaderProgram();
    SE_ColorExtractShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc);
    ~SE_ColorExtractShaderProgram();
    //index start from 0
    GLint getTextureUniformLoc(int index);
    GLint getTexCombineModeUniformLoc()
    {
        return m_u_tex_combine_mode_loc;
    }
    GLint getColorOpModeUniformLoc()
    {
        return m_u_color_op_mode_loc;
    }
    GLint getColorChannelIndexUniformLoc(int index);
    // index start from 1
    GLint getTexCoordIndexUniformLoc(int index);
    GLint getMarkColorUniformLoc(int index);
        // index start from 0
    GLint getTextureCoordAttributeLoc(int index);
protected:
    void link();
private:
    GLint m_a_tex_coord0_loc;
    GLint m_a_tex_coord1_loc;
    GLint m_a_tex_coord2_loc;
    GLint m_a_tex_coord3_loc;
    GLint m_u_texture0_loc;
    GLint m_u_texture1_loc;
    GLint m_u_texture2_loc;
    GLint m_u_texture3_loc;
    GLint m_u_tex_combine_mode_loc;
    GLint m_u_color_op_mode_loc;
    //tex_coord_index indicate which texture coordinate will be used in
    //this texture. for example: m_u_tex0_coord_index is 1 that means texture0 use m_a_tex_coord1
    GLint m_u_tex0_coord_index_loc;
    GLint m_u_tex1_coord_index_loc;
    GLint m_u_tex2_coord_index_loc;
    GLint m_u_tex3_coord_index_loc;
    GLint m_u_colora_loc;
    GLint m_u_colorr_loc;
    GLint m_u_colorg_loc;
    GLint m_u_colorb_loc;
    GLint m_u_rchannelindex_loc;
    GLint m_u_gchannelindex_loc;
    GLint m_u_bchannelindex_loc;
    GLint m_u_achannelindex_loc;
};
class SE_ColorEffectShaderProgram : public SE_ShaderProgram
{
    DECLARE_OBJECT(SE_ColorEffectShaderProgram)
public:
    SE_ColorEffectShaderProgram();
    GLint getTexCoordAttribLoc()
    {
        return m_a_tex_coord0;
    }
    // 0 : background
    // 1 : channel
    // 2 - 5 : texr, texg, texb, texa
    GLint getTextureUniformLoc(int index)
    {
        GLint ret = -1;
        switch(index)
        {
        case 0:
            ret = m_u_texture_background;
            break;
        case 1:
            ret = m_u_texture_channel;
            break;
        case 2:
            ret = m_u_texture_texr;
            break;
        case 3:
            ret = m_u_texture_texg;
            break;
        case 4:
            ret = m_u_texture_texb;
            break;
        case 5:
            ret = m_u_texture_texa;
            break;
        default:
            break;
        }
        return ret;
    }
    // 0, 1, 2, 3 | r g b a
    GLint getHasTextureUniformLoc(int index)
    {
        GLint ret = -1;
        switch(index)
        {
        case 0:
            ret = m_u_has_texr;
            break;
        case 1:
            ret = m_u_has_texg;
            break;
        case 2:
            ret = m_u_has_texb;
            break;
        case 3:
            ret = m_u_has_texa;
            break;
        default:
            break;
        }
        return ret;
    }
    // 0, 1, 2, 3, 4 | markr markg markb marka backgroudnalpha
    GLint getMarkAlphaUniformLoc(int index)
    {
        switch(index)
        {
        case 0:
            return m_u_markr_alpha;
        case 1:
            return m_u_markg_alpha;
        case 2:
            return m_u_markb_alpha;
        case 3:
            return m_u_marka_alpha;
        case 4:
            return m_u_background_alpha;
        default:
            return -1;
        }
    }
    // 0, 1, 2, 3 | r g b a
    GLint getMarkFunctionUniformLoc(int index)
    {
        switch(index)
        {
        case 0:
            return m_u_markr_fn;
        case 1:
            return m_u_markg_fn;
        case 2:
            return m_u_markb_fn;
        case 3:
            return m_u_marka_fn;
        default:
            return -1;
        }
    }
    // 0, 1, 2, 3 | r g b a
    GLint getMarkColorUniformLoc(int index)
    {
        switch(index)
        {
        case 0:
            return m_u_colorr;
        case 1:
            return m_u_colorg;
        case 2:
            return m_u_colorb;
        case 3:
            return m_u_colora;
        default:
            return -1;
        }
    }
protected:
    virtual void link();
private:
    GLint m_a_tex_coord0;
    GLint m_u_texture_background;
    GLint m_u_texture_channel;
    GLint m_u_texture_texr;
    GLint m_u_texture_texg;
    GLint m_u_texture_texb;
    GLint m_u_texture_texa;
    GLint m_u_has_texr;
    GLint m_u_has_texg;
    GLint m_u_has_texb;
    GLint m_u_has_texa;
    GLint m_u_markr_alpha;
    GLint m_u_markg_alpha;
    GLint m_u_markb_alpha;
    GLint m_u_marka_alpha;
    GLint m_u_background_alpha;
    GLint m_u_markr_fn;
    GLint m_u_markg_fn;
    GLint m_u_markb_fn;
    GLint m_u_marka_fn;
    GLint m_u_colorr;
    GLint m_u_colorg;
    GLint m_u_colorb;
    GLint m_u_colora;
};

class SE_FadeInOutShaderProgram : public SE_ShaderProgram
{
    DECLARE_OBJECT(SE_FadeInOutShaderProgram)
public:
    SE_FadeInOutShaderProgram();
    SE_FadeInOutShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc);
    ~SE_FadeInOutShaderProgram();


    GLint getTexCoordAttributeLoc()
    {
        return m_a_tex_coord_loc;
    }
    GLint getTextureUniformLoc()
    {
        return m_u_texture_loc;
    }
    GLint getColorUniformLoc()
    {
        return m_u_color_loc;
    }
    GLint getShadingModeUniformLoc()
    {
        return m_u_shading_mode_loc;
    }

    GLint getAlphaUniformLoc()
    {
        return m_u_alpha_loc;
    }    
protected:
    virtual void link();
private:    

    GLint m_u_color_loc;
    GLint m_a_tex_coord_loc;
    GLint m_u_texture_loc;
    GLint m_u_shading_mode_loc;
    //single alpha 
    GLint m_u_alpha_loc;    
};

class SE_SkeletalAnimationShaderProgram : public SE_ShaderProgram
{
    DECLARE_OBJECT(SE_SkeletalAnimationShaderProgram)
public:
    SE_SkeletalAnimationShaderProgram();
    SE_SkeletalAnimationShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc);
    ~SE_SkeletalAnimationShaderProgram();


    GLint getTexCoordAttributeLoc()
    {
        return m_a_tex_coord_loc;
    }
    GLint getTextureUniformLoc()
    {
        return m_u_texture_loc;
    }
    GLint getColorUniformLoc()
    {
        return m_u_color_loc;
    }
    GLint getShadingModeUniformLoc()
    {
        return m_u_shading_mode_loc;
    }

    GLint getAlphaUniformLoc()
    {
        return m_u_alpha_loc;
    }

    GLint getBoneIndexAttributeLoc()
    {
        return m_u_boneindex_loc;
    }
    GLint getBoneWeightAttributeLoc()
    {
        return m_u_boneweight_loc;
    }
    GLint getBoneMatrixUniformLoc()
    {
        return m_u_bonematrix_loc;
    }
    GLint getInversMatrixUniformLoc()
    {
        return m_u_inversmatrix_loc;
    }
protected:
    virtual void link();
private:    

    GLint m_u_color_loc;
    GLint m_a_tex_coord_loc;
    GLint m_u_texture_loc;
    GLint m_u_shading_mode_loc;
    //single alpha 
    GLint m_u_alpha_loc;

    //debug code
    GLint m_u_boneindex_loc;
    GLint m_u_boneweight_loc;
    GLint m_u_bonematrix_loc;
    GLint m_u_inversmatrix_loc;
};

class SE_SimpleLightingShaderProgram : public SE_ShaderProgram
{
    DECLARE_OBJECT(SE_SimpleLightingShaderProgram)
public:
    SE_SimpleLightingShaderProgram();
    SE_SimpleLightingShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc);
    ~SE_SimpleLightingShaderProgram();


    GLint getTexCoordAttributeLoc()
    {
        return m_a_tex_coord_loc;
    }
    GLint getTextureUniformLoc()
    {
        return m_u_texture_loc;
    }
    GLint getColorUniformLoc()
    {
        return m_u_color_loc;
    }
    GLint getShadingModeUniformLoc()
    {
        return m_u_shading_mode_loc;
    }

    GLint getLightPosUniformLoc()
    {
        return m_u_lightPos_loc;
    }  

    GLint getEyePosUniformLoc()
    {
        return m_u_eyePos_loc;
    }

    GLint getModelToWorldUniformLoc()
    {
        return u_m2w_matrix_loc;
    }

    GLint getMaterialAmbientUniformLoc()
    {
        return m_u_ambient_loc;
    }

    GLint getMaterialDiffuseUniformLoc()
    {
        return m_u_diffuse_loc;
    }

    GLint getMaterialSpecularUniformLoc()
    {
        return m_u_specular_loc;
    }

    GLint getMaterialShinessUniformLoc()
    {
        return m_u_shiness_loc;
    }
protected:
    virtual void link();
private:    

    GLint m_u_color_loc;
    GLint m_a_tex_coord_loc;
    GLint m_u_texture_loc;
    GLint m_u_shading_mode_loc;

    //light direction 
    GLint m_u_lightPos_loc;
    GLint m_u_eyePos_loc;
    GLint u_m2w_matrix_loc;

    //material
    GLint m_u_ambient_loc;
    GLint m_u_diffuse_loc;
    GLint m_u_specular_loc;
    GLint m_u_shiness_loc;
};

class SE_NormalMapShaderProgram : public SE_SimpleLightingShaderProgram
{
    DECLARE_OBJECT(SE_NormalMapShaderProgram)
public:
    SE_NormalMapShaderProgram();
    SE_NormalMapShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc);
    ~SE_NormalMapShaderProgram();
    
    //Normal map
    GLint getTangentSpaceAttributeLoc()
    {
        return m_u_vertex_tangent_loc;
    }

    GLint getNormalMapUniformLoc()
    {
        return m_u_normal_map_loc;
    }
    
protected:
    virtual void link();
private:
    GLint m_u_vertex_tangent_loc;
    GLint m_u_normal_map_loc;
    
};
#endif
