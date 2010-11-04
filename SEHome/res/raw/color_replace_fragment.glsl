precision mediump float;
const int DEFAULT_COLOR = 0;
const int REPLACE_R = 1;
const int REPLACE_G = 2;
const int REPLACE_B = 3;
const int REPLACE_A = 4;

const int REPLACE_RG = 5;
const int REPLACE_RB = 6;
const int REPLACE_RA = 7;
const int REPLACE_GB = 8;
const int REPLACE_GA = 9;
const int REPLACE_BA = 10;

const int REPLACE_RGB = 11;
const int REPLACE_RGA = 12;
const int REPLACE_GBA = 13;
const int REPLACE_RBA = 14;
const int REPLACE_RGBA = 15;
const int NO_REPLACE = 16;
/////////////////////////////
const int TEX0 = 0;
const int TEX1 = 1;
const int TEX2 = 2;
const int TEX3 = 3;
///////////////////////////////
varying vec2 v_tex_coord0;
varying vec2 v_tex_coord1;
varying vec2 v_tex_coord2;
varying vec2 v_tex_coord3;

uniform sampler2D u_texture0;
uniform sampler2D u_texture1;
uniform sampler2D u_texture2;
uniform sampler2D u_texture3;
uniform int u_tex0_coord_index;
uniform int u_tex1_coord_index;
uniform int u_tex2_coord_index;
uniform int u_tex3_coord_index;
uniform int u_color_op_mode;
uniform int u_rchannelindex;//value is 0 , 1, 2, 3    0: r color 1 : g color 2 : b color 3 a color
uniform int u_gchannelindex;
uniform int u_bchannelindex;
uniform int u_achannelindex;
vec4 defaultColor = vec4(0, 1, 0, 1);
float getReplaceValue(vec4 color, int index)
{
    if(index == 0)
    {
        return color.r;
    } 
    else if(index == 1)
    {
        return color.g;
    }
    else if(index == 2)
    {
        return color.b;
    }
    else if(index == 3)
    {
        return color.a;
    }
    else
    {}
    return color.a;
}
vec2 getTexCoord(int tex)
{
    int index = TEX0;
    if(tex == TEX0)
    {
        index = u_tex0_coord_index;
    }
    else if(tex == TEX1)
    {
        index = u_tex1_coord_index;
    }
    else if(tex == TEX2)
    {
        index = u_tex2_coord_index;
    }
    else if(tex == TEX3)
    {
        index = u_tex3_coord_index;
    }
    if(index == TEX0)
    {
        return v_tex_coord0;
    }
    else if(index == TEX1)
    {
        return v_tex_coord1;
    } 
    else if(index == TEX2)
    {
        return v_tex_coord2;
    } 
    else if(index == TEX3)
    {
        return v_tex_coord3;
    }
    else
    {}     
    return v_tex_coord0;
}
void main()
{
    if(u_color_op_mode == DEFAULT_COLOR)
    {
        gl_FragColor = defaultColor;
    }
    else if(u_color_op_mode == NO_REPLACE)
    {
        vec2 texcoord = v_tex_coord0;
        gl_FragColor = texture2D(u_texture0, texcoord);
    }   
    else if(u_color_op_mode == REPLACE_R)
    {
        vec2 texcoord0 = getTexCoord(TEX0);
        vec2 texcoord1 = getTexCoord(TEX1);
        vec4 color1 = texture2D(u_texture0, texcoord0);
        vec4 color2 = texture2D(u_texture1, texcoord1);
        float c = getReplaceValue(color2, u_rchannelindex);
        gl_FragColor = vec4(c, color1.g, color1.b, color1.a);
    }
    else if(u_color_op_mode == REPLACE_G)
    {
        vec2 texcoord0 = getTexCoord(TEX0);
        vec2 texcoord1 = getTexCoord(TEX1);
        vec4 color1 = texture2D(u_texture0, texcoord0);
        vec4 color2 = texture2D(u_texture1, texcoord1);
        float c = getReplaceValue(color2, u_gchannelindex);
        gl_FragColor = vec4(color1.r, c, color1.b, color1.a);        
    }
    else if(u_color_op_mode == REPLACE_B)
    {
        vec2 texcoord0 = getTexCoord(TEX0);
        vec2 texcoord1 = getTexCoord(TEX1);
        vec4 color1 = texture2D(u_texture0, texcoord0);
        vec4 color2 = texture2D(u_texture1, texcoord1);
        float c = getReplaceValue(color2, u_bchannelindex);
        gl_FragColor = vec4(color1.r, color1.g, c, color1.a);
    }
    else if(u_color_op_mode == REPLACE_A)
    {
        vec2 texcoord0 = getTexCoord(TEX0);
        vec2 texcoord1 = getTexCoord(TEX1);
        vec4 color1 = texture2D(u_texture0, texcoord0);
        vec4 color2 = texture2D(u_texture1, texcoord1);
        float c = getReplaceValue(color2, u_achannelindex);
        gl_FragColor = vec4(color1.r, color1.g, color1.b, c);
    }
    else if(u_color_op_mode == REPLACE_RG)
    {
        vec2 texcoord0 = getTexCoord(TEX0);
        vec2 texcoord1 = getTexCoord(TEX1);
        vec2 texcoord2 = getTexCoord(TEX2);
        vec4 color1 = texture2D(u_texture0, texcoord0);
        vec4 color2 = texture2D(u_texture1, texcoord1);
        vec4 color3 = texture2D(u_texture2, texcoord2);
        float c1 = getReplaceValue(color2, u_rchannelindex);
        float c2 = getReplaceValue(color3, u_gchannelindex);
        gl_FragColor = vec4(c1, c2, color1.b, color1.a);
    }
    else if(u_color_op_mode == REPLACE_RB)
    {
        vec2 texcoord0 = getTexCoord(TEX0);
        vec2 texcoord1 = getTexCoord(TEX1);
        vec2 texcoord2 = getTexCoord(TEX2);
        vec4 color1 = texture2D(u_texture0, texcoord0);
        vec4 color2 = texture2D(u_texture1, texcoord1);
        vec4 color3 = texture2D(u_texture2, texcoord2);
        float c1 = getReplaceValue(color2, u_rchannelindex);
        float c2 = getReplaceValue(color3, u_bchannelindex);
        gl_FragColor = vec4(c1, color1.g, c2, color1.a);
    }
    else if(u_color_op_mode == REPLACE_RA)
    {
        vec2 texcoord0 = getTexCoord(TEX0);
        vec2 texcoord1 = getTexCoord(TEX1);
        vec2 texcoord2 = getTexCoord(TEX2);
        vec4 color1 = texture2D(u_texture0, texcoord0);
        vec4 color2 = texture2D(u_texture1, texcoord1);
        vec4 color3 = texture2D(u_texture2, texcoord2);
        float c1 = getReplaceValue(color2, u_rchannelindex);
        float c2 = getReplaceValue(color3, u_achannelindex);
        gl_FragColor = vec4(c1, color1.g, color1.b, c2);
    }
    else if(u_color_op_mode == REPLACE_GB)
    {
        vec2 texcoord0 = getTexCoord(TEX0);
        vec2 texcoord1 = getTexCoord(TEX1);
        vec2 texcoord2 = getTexCoord(TEX2);
        vec4 color1 = texture2D(u_texture0, texcoord0);
        vec4 color2 = texture2D(u_texture1, texcoord1);
        vec4 color3 = texture2D(u_texture2, texcoord2);
        float c1 = getReplaceValue(color2, u_gchannelindex);
        float c2 = getReplaceValue(color3, u_bchannelindex);
        gl_FragColor = vec4(color1.r, c1, c2, color1.a);
    }
    else if(u_color_op_mode == REPLACE_GA)
    {
        vec2 texcoord0 = getTexCoord(TEX0);
        vec2 texcoord1 = getTexCoord(TEX1);
        vec2 texcoord2 = getTexCoord(TEX2);
        vec4 color1 = texture2D(u_texture0, texcoord0);
        vec4 color2 = texture2D(u_texture1, texcoord1);
        vec4 color3 = texture2D(u_texture2, texcoord2);
        float c1 = getReplaceValue(color2, u_gchannelindex);
        float c2 = getReplaceValue(color3, u_achannelindex);
        gl_FragColor = vec4(color1.r, c1, color1.b, c2);
    }
    else if(u_color_op_mode == REPLACE_BA)
    {
        vec2 texcoord0 = getTexCoord(TEX0);
        vec2 texcoord1 = getTexCoord(TEX1);
        vec2 texcoord2 = getTexCoord(TEX2);
        vec4 color1 = texture2D(u_texture0, texcoord0);
        vec4 color2 = texture2D(u_texture1, texcoord1);
        vec4 color3 = texture2D(u_texture2, texcoord2);
        float c1 = getReplaceValue(color2, u_bchannelindex);
        float c2 = getReplaceValue(color3, u_achannelindex);
        gl_FragColor = vec4(color1.r, color1.g, c1, c2);
    }
    else if(u_color_op_mode == REPLACE_RGB)
    {
        vec2 texcoord0 = getTexCoord(TEX0);
        vec2 texcoord1 = getTexCoord(TEX1);
        vec2 texcoord2 = getTexCoord(TEX2);
        vec2 texcoord3 = getTexCoord(TEX3);
        vec4 color1 = texture2D(u_texture0, texcoord0);
        vec4 color2 = texture2D(u_texture1, texcoord1);
        vec4 color3 = texture2D(u_texture2, texcoord2);
        vec4 color4 = texture2D(u_texture3, texcoord3);
        float c1 = getReplaceValue(color2, u_rchannelindex);
        float c2 = getReplaceValue(color3, u_gchannelindex);
        float c3 = getReplaceValue(color4, u_bchannelindex);
        gl_FragColor = vec4(c1, c2, c3, color1.a);
    }
    else if(u_color_op_mode == REPLACE_RGA)
    {
        vec2 texcoord0 = getTexCoord(TEX0);
        vec2 texcoord1 = getTexCoord(TEX1);
        vec2 texcoord2 = getTexCoord(TEX2);
        vec2 texcoord3 = getTexCoord(TEX3);
        vec4 color1 = texture2D(u_texture0, texcoord0);
        vec4 color2 = texture2D(u_texture1, texcoord1);
        vec4 color3 = texture2D(u_texture2, texcoord2);
        vec4 color4 = texture2D(u_texture3, texcoord3);
        float c1 = getReplaceValue(color2, u_rchannelindex);
        float c2 = getReplaceValue(color3, u_gchannelindex);
        float c3 = getReplaceValue(color4, u_achannelindex);
        gl_FragColor = vec4(c1, c2, color1.b, c3);
    }
    else if(u_color_op_mode == REPLACE_GBA)
    {
        vec2 texcoord0 = getTexCoord(TEX0);
        vec2 texcoord1 = getTexCoord(TEX1);
        vec2 texcoord2 = getTexCoord(TEX2);
        vec2 texcoord3 = getTexCoord(TEX3);
        vec4 color1 = texture2D(u_texture0, texcoord0);
        vec4 color2 = texture2D(u_texture1, texcoord1);
        vec4 color3 = texture2D(u_texture2, texcoord2);
        vec4 color4 = texture2D(u_texture3, texcoord3);
        float c1 = getReplaceValue(color2, u_gchannelindex);
        float c2 = getReplaceValue(color3, u_bchannelindex);
        float c3 = getReplaceValue(color4, u_achannelindex);
        gl_FragColor = vec4(color1.r, c1, c2, c3);
    }
    else if(u_color_op_mode == REPLACE_RBA)
    { 
        vec2 texcoord0 = getTexCoord(TEX0);
        vec2 texcoord1 = getTexCoord(TEX1);
        vec2 texcoord2 = getTexCoord(TEX2);
        vec2 texcoord3 = getTexCoord(TEX3);
        vec4 color1 = texture2D(u_texture0, texcoord0);
        vec4 color2 = texture2D(u_texture1, texcoord1);
        vec4 color3 = texture2D(u_texture2, texcoord2);
        vec4 color4 = texture2D(u_texture3, texcoord3);
        float c1 = getReplaceValue(color2, u_rchannelindex);
        float c2 = getReplaceValue(color3, u_bchannelindex);
        float c3 = getReplaceValue(color4, u_achannelindex);
        gl_FragColor = vec4(c1, color1.g, c2, c3);
    }
    else if(u_color_op_mode == REPLACE_RGBA)
    {
        vec2 texcoord0 = getTexCoord(TEX0);
        vec2 texcoord1 = getTexCoord(TEX1);
        vec2 texcoord2 = getTexCoord(TEX2);
        vec2 texcoord3 = getTexCoord(TEX3);
        vec4 color1 = texture2D(u_texture0, texcoord0);
        vec4 color2 = texture2D(u_texture1, texcoord1);
        vec4 color3 = texture2D(u_texture2, texcoord2);
        vec4 color4 = texture2D(u_texture3, texcoord3);
        float c1 = getReplaceValue(color1, u_rchannelindex);
        float c2 = getReplaceValue(color2, u_gchannelindex);
        float c3 = getReplaceValue(color3, u_bchannelindex);
        float c4 = getReplaceValue(color4, u_achannelindex);
        gl_FragColor = vec4(c1,  c2, c3, c4);
    }
}
