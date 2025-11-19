package com.simply.ai.server.web.service.impl;

import com.simply.ai.server.manager.enums.ImageResponseCodeEnum;
import com.simply.ai.server.manager.manager.ImageManager;
import com.simply.ai.server.manager.model.request.Gpt4oImageGenerateRequest;
import com.simply.ai.server.manager.model.response.ImageGenerateResponse;
import com.simply.ai.server.web.model.dto.request.Gpt4oImageGenerateDTO;
import com.simply.ai.server.web.service.Gpt4oImageService;
import com.simply.common.core.exception.BaseException;
import com.simply.common.core.exception.error.ThirdpartyErrorType;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class Gpt4oImageServiceImpl implements Gpt4oImageService {

    @Autowired
    private ImageManager imageManager;

    @Override
    public void gpt4oImageGenerate(Gpt4oImageGenerateDTO gpt4oImageGenerateDTO) {

        // 实现视频生成逻辑
        Gpt4oImageGenerateRequest request = new Gpt4oImageGenerateRequest();

        BeanUtils.copyProperties(gpt4oImageGenerateDTO, request);

        ImageGenerateResponse response = imageManager.gpt4oImageGenerate(request);

        if(!ImageResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMessage());
        }

    }
}
