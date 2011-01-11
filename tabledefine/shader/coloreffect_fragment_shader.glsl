precision mediump float;
const int FN_MULTIPLY = 0;
const int FN_ADD = 1;
const int TEXTURE_FN_REPLACE = 0;
const int TEXTURE_FN_MULTIPLY = 1;
const int TEXTURE_FN_ADD = 2;
varying vec2 v_tex_coord0;
uniform sampler2D u_texture_background;
uniform sampler2D u_texture_channel;
uniform sampler2D u_texture_texr;
uniform sampler2D u_texture_texg;
uniform sampler2D u_texture_texb;
uniform sampler2D u_texture_texa;
uniform int u_has_texr;
uniform int u_has_texg;
uniform int u_has_texb;
uniform int u_has_texa;
uniform float u_markr_alpha;
uniform float u_markg_alpha;
uniform float u_markb_alpha;
uniform float u_marka_alpha;
uniform float u_background_alpha;

uniform int u_markr_fn;
uniform int u_markg_fn;
uniform int u_markb_fn;
uniform int u_marka_fn;

uniform int u_texr_fn;
uniform int u_texg_fn;
uniform int u_texb_fn;
uniform int u_texa_fn;

uniform int u_has_markr;
uniform int u_has_markg;
uniform int u_has_markb;
uniform int u_has_marka;

uniform vec3 u_colora;
uniform vec3 u_colorr;
uniform vec3 u_colorg;
uniform vec3 u_colorb;
vec3 calculateColor(vec3 backgroundColor, float alpha, vec3 foregroundColor, int fn)
{
    if(fn == FN_MULTIPLY)
    {
        float r = alpha * backgroundColor.r * foregroundColor.r;
        float g = alpha * backgroundColor.g * foregroundColor.g;
        float b = alpha * backgroundColor.b * foregroundColor.b;
        r = (1.0 - alpha) * backgroundColor.r + alpha * r;
        g = (1.0 - alpha) * backgroundColor.g + alpha * g;
        b = (1.0 - alpha) * backgroundColor.b + alpha * b;
        return vec3(r, g, b);
    }
    else if(fn == FN_ADD)
    {
        float r = (foregroundColor.r + backgroundColor.r) > 1.0 ? 1.0 : (foregroundColor.r + backgroundColor.r);
        float g = (foregroundColor.g + backgroundColor.g) > 1.0 ? 1.0 : (foregroundColor.g + backgroundColor.g);
        float b = (foregroundColor.b + backgroundColor.b) > 1.0 ? 1.0 : (foregroundColor.b + backgroundColor.b);
        r = (1.0 - alpha) * backgroundColor.r + alpha * r;
        g = (1.0 - alpha) * backgroundColor.g + alpha * g;
        b = (1.0 - alpha) * backgroundColor.b + alpha * b;
        return vec3(r, g, b);
    }
    return vec3(0, 0, 0);
}
vec3 calculateTextureColor(vec3 foregroundColor, sampler2D t, vec2 coord, int texFn)
{
    vec4 texColor = texture2D(t, coord); 
    vec3 color;
    if(texFn ==  TEXTURE_FN_MULTIPLY)
    {
        color = foregroundColor * texColor.rgb;
    }
    else if(texFn == TEXTURE_FN_ADD)
    {
        float tmp =  foregroundColor.r + texColor.r;
        color.r = tmp > 1.0 ? 1.0 : tmp;
        tmp = foregroundColor.g + texColor.g;
        color.g = tmp > 1.0 ? 1.0 : tmp;
        tmp = foregroundColor.b + texColor.b;
        color.b = tmp > 1.0 ? 1.0 : tmp;
    }
    else
    {
        color = texColor.rgb;
    }
    return color;
}
void main()
{
//////////////
/*
    vec4 color = texture2D(u_texture_background, v_tex_coord0);
    color = texture2D(u_texture_channel, v_tex_coord0);
    color = texture2D(u_texture_texr, v_tex_coord0);
    color = texture2D(u_texture_texg, v_tex_coord0);
    color = texture2D(u_texture_texb, v_tex_coord0);
    color = texture2D(u_texture_texa, v_tex_coord0);
    int fn = u_has_texr;
    int n = u_has_texg;
    n = u_has_texb;
    n = u_has_texa;
    n = u_has_markr;
    n = u_has_markg;
    n = u_has_markb;
    n = u_has_marka;
    vec3 r = u_colora;
    r = u_colorg;
    r = u_colorb;
    r = u_colora;
    n = u_markr_fn;
    n = u_markg_fn; 
    n = u_markb_fn;
    n = u_marka_fn;
    n = u_texr_fn;
    n = u_texg_fn;
    n = u_texb_fn;
    n = u_texa_fn;
    float a = u_markr_alpha;
    a = u_markg_alpha;
    a = u_markb_alpha;
    a = u_marka_alpha;
    a = u_background_alpha;

    gl_FragColor = color;
*/
//////////////////////////////////////////
    vec4 backgroundColor = texture2D(u_texture_background, v_tex_coord0);
    vec4 channelColor = texture2D(u_texture_channel, v_tex_coord0);
    vec3 foregroundColor = vec3(0, 0, 0);
    vec3 color = backgroundColor.rgb;
    if(u_has_markr == 1)
    {
        foregroundColor = u_colorr;
        if(u_has_texr == 1)
        {
            foregroundColor = calculateTextureColor(foregroundColor, u_texture_texr, v_tex_coord0, u_texr_fn);
        }
        color = calculateColor(color, channelColor.r * u_markr_alpha, foregroundColor, u_markr_fn);
    }
    if(u_has_markg == 1)
    {
        foregroundColor = u_colorg;
        if(u_has_texg == 1)
        {
            foregroundColor = calculateTextureColor(foregroundColor, u_texture_texg, v_tex_coord0, u_texg_fn);
        }
        color = calculateColor(color, channelColor.g  * u_markg_alpha, foregroundColor, u_markg_fn);
    }

    if(u_has_markb == 1)
    {
        foregroundColor = u_colorb;
        if(u_has_texb == 1)
        {
            foregroundColor = calculateTextureColor(foregroundColor, u_texture_texb, v_tex_coord0, u_texb_fn);
        }
        color = calculateColor(color, channelColor.b * u_markb_alpha, foregroundColor, u_markb_fn);
    }
    //gl_FragColor = vec4(color, backgroundColor.a * u_background_alpha);
    if(u_has_marka == 1)
    {
        foregroundColor = u_colora;
        if(u_has_texa == 1)
        {
            foregroundColor = calculateTextureColor(foregroundColor, u_texture_texa, v_tex_coord0, u_texa_fn);
        }
        color = calculateColor(color, channelColor.a * u_marka_alpha, foregroundColor, u_marka_fn);
    }
    gl_FragColor = vec4(color, backgroundColor.a * u_background_alpha);
}
