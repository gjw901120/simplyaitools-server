package com.simply.ai.server.manager.manager.impl;

import com.simply.ai.server.manager.feign.client.ImageFeignClient;
import com.simply.ai.server.manager.manager.ImageManager;
import com.simply.ai.server.manager.model.request.FluxKontextImageRequest;
import com.simply.ai.server.manager.model.request.Gpt4oImageGenerateRequest;
import com.simply.ai.server.manager.model.request.NanoBananaEditRequest;
import com.simply.ai.server.manager.model.request.NanoBananaGenerateRequest;
import com.simply.ai.server.manager.model.response.ImageGenerateResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ImageManagerImpl implements ImageManager {

    @Autowired
    private ImageFeignClient imageFeignClient;


    /**
     * 生成GPT-4o图像
     */
    @Override
    public ImageGenerateResponse generateImage(Gpt4oImageGenerateRequest request) {
        return imageFeignClient.generateImage(request);
    }

    /**
     * 生成/编辑flux-kontext图像
     */
    @Override
    public ImageGenerateResponse generateOrEditImage(FluxKontextImageRequest request) {
        return imageFeignClient.generateOrEditImage(request);
    }

    /**
     * 生成图像
     */
    @Override
    public ImageGenerateResponse generateImage(NanoBananaGenerateRequest request) {
        return imageFeignClient.generateImage(request);
    }

    /**
     * 编辑图像
     */
    @Override
    public ImageGenerateResponse editImage(NanoBananaEditRequest request) {
        return imageFeignClient.editImage(request);
    }

}
