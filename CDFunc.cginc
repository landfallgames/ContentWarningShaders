inline float3 bump3y (float3 x, float3 yoffset)
{
    float3 y = 1 - x * x;
    y = saturate(y-yoffset);
    return y;
}

float3 spectral_zucconi6 (float w)
{
    // w: [400, 700]
    // x: [0,   1]
    float x = saturate((w - 400.0)/ 300.0);
    const float3 c1 = float3(3.54585104, 2.93225262, 2.41593945);
    const float3 x1 = float3(0.69549072, 0.49228336, 0.27699880);
    const float3 y1 = float3(0.02312639, 0.15225084, 0.52607955);
    const float3 c2 = float3(3.90307140, 3.21182957, 3.96587128);
    const float3 x2 = float3(0.11748627, 0.86755042, 0.66077860);
    const float3 y2 = float3(0.84897130, 0.88445281, 0.73949448);
    return
        bump3y(c1 * (x - x1), y1) +
        bump3y(c2 * (x - x2), y2) ;
}

float3 CDRefraction(float3 viewDir, float3 lightDirection, float distance, float2 uv, float3 defaultColor)
{
    uv = uv * 2 -1;
    float2 uv_orthogonal = normalize(uv);
    float3 uv_tangent = float3(-uv_orthogonal.y, 0, uv_orthogonal.x);

    
    float3 L =lightDirection;
    float3 V = viewDir;
    float3 T = uv_tangent;
    float d = distance;
    float cos_ThetaL = dot(L, T);
    float cos_ThetaV = dot(V, T);
    float u = abs(cos_ThetaL - cos_ThetaV);

    if (u == 0)
        return defaultColor;
    
    float3 color = 0;
    for (int n = 1; n <= 4; n++)
    {
        float wavelength = u * d / n;
        color += spectral_zucconi6(wavelength);
    }
    color = saturate(color);

    if(color.r < 0.15 && color.g < 0.15 && color.b < 0.15)
    {
        return defaultColor;
    }
    
    return lerp(defaultColor, color, 0.15);
}
