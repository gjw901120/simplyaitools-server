package com.simply.ai.server.web.service.impl;

import com.simply.ai.server.manager.enums.ImageResponseCodeEnum;
import com.simply.ai.server.manager.manager.ImageManager;
import com.simply.ai.server.manager.model.request.FluxKontextImageRequest;
import com.simply.ai.server.manager.model.request.Gpt4oImageGenerateRequest;
import com.simply.ai.server.manager.model.response.ImageGenerateResponse;
import com.simply.ai.server.web.model.dto.request.FluxKontextGenerateDTO;
import com.simply.ai.server.web.model.dto.request.Gpt4oImageGenerateDTO;
import com.simply.ai.server.web.service.FluxKontextService;
import com.simply.ai.server.web.service.Gpt4oImageService;
import com.simply.common.core.exception.BaseException;
import com.simply.common.core.exception.error.ThirdpartyErrorType;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class FluxKontextServiceImpl implements FluxKontextService {

    @Autowired
    private ImageManager imageManager;

    @Override
    public void fluxKontextGenerate(FluxKontextGenerateDTO fluxKontextGenerateDTO) {

        // 实现视频生成逻辑
        FluxKontextImageRequest request = new FluxKontextImageRequest();

        BeanUtils.copyProperties(fluxKontextGenerateDTO, request);

        ImageGenerateResponse response = imageManager.fluxKontextGenerate(request);

        if(!ImageResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMessage());
        }

    }
}
