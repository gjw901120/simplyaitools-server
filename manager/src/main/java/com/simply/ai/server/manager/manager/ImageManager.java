package com.simply.ai.server.manager.manager;

import com.simply.ai.server.manager.model.request.FluxKontextImageRequest;
import com.simply.ai.server.manager.model.request.Gpt4oImageGenerateRequest;
import com.simply.ai.server.manager.model.request.NanoBananaEditRequest;
import com.simply.ai.server.manager.model.request.NanoBananaGenerateRequest;
import com.simply.ai.server.manager.model.response.ImageGenerateResponse;


public interface ImageManager {

    /**
     * 生成GPT-4o图像
     */
    ImageGenerateResponse generateImage(Gpt4oImageGenerateRequest request);

    /**
     * 生成/编辑flux-kontext图像
     */
    ImageGenerateResponse generateOrEditImage(FluxKontextImageRequest request);

    /**
     * 生成图像
     */
    ImageGenerateResponse generateImage(NanoBananaGenerateRequest request);

    /**
     * 编辑图像
     */
    ImageGenerateResponse editImage(NanoBananaEditRequest request);

}
