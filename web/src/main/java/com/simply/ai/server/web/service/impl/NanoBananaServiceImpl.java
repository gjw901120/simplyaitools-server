package com.simply.ai.server.web.service.impl;

import com.simply.ai.server.manager.enums.ImageResponseCodeEnum;
import com.simply.ai.server.manager.enums.NanoBananaAspectRatioEnum;
import com.simply.ai.server.manager.enums.NanoBananaOutputFormatEnum;
import com.simply.ai.server.manager.manager.ImageManager;
import com.simply.ai.server.manager.model.request.NanoBananaEditRequest;
import com.simply.ai.server.manager.model.request.NanoBananaGenerateRequest;
import com.simply.ai.server.manager.model.response.ImageGenerateResponse;
import com.simply.ai.server.web.model.dto.request.NanoBananaEditDTO;
import com.simply.ai.server.web.model.dto.request.NanoBananaGenerateDTO;
import com.simply.ai.server.web.service.NanoBananaService;
import com.simply.common.core.exception.BaseException;
import com.simply.common.core.exception.error.ThirdpartyErrorType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class NanoBananaServiceImpl implements NanoBananaService {

    @Autowired
    private ImageManager imageManager;

    @Override
    public void nanoBananaGenerate(NanoBananaGenerateDTO nanoBananaGenerateDTO) {

        // 实现视频生成逻辑
        NanoBananaGenerateRequest request = new NanoBananaGenerateRequest();
        NanoBananaGenerateRequest.Input input = new NanoBananaGenerateRequest.Input();

        request.setModel(nanoBananaGenerateDTO.getModel());
        input.setOutputFormat(NanoBananaOutputFormatEnum.getByFormat(nanoBananaGenerateDTO.getOutputFormat()));
        input.setPrompt(nanoBananaGenerateDTO.getPrompt());
        input.setImageSize(NanoBananaAspectRatioEnum.getByRatio(nanoBananaGenerateDTO.getImageSize()));
        request.setInput(input);

        ImageGenerateResponse response = imageManager.nanoBananaGenerate(request);

        if(!ImageResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMessage());
        }

    }

    @Override
    public void nanoBananaEdit(NanoBananaEditDTO nanoBananaEditDTO) {

        // 实现视频生成逻辑
        NanoBananaEditRequest request = new NanoBananaEditRequest();
        NanoBananaEditRequest.Input  input = new NanoBananaEditRequest.Input();

        request.setModel(nanoBananaEditDTO.getModel());
        input.setImageSize(NanoBananaAspectRatioEnum.getByRatio(nanoBananaEditDTO.getImageSize()));
        List<String> imageUrls = new ArrayList<>();
        input.setImageUrls(imageUrls);
        input.setOutputFormat(NanoBananaOutputFormatEnum.getByFormat(nanoBananaEditDTO.getOutputFormat()));
        input.setPrompt(nanoBananaEditDTO.getPrompt());
        request.setInput(input);

        ImageGenerateResponse response = imageManager.nanoBananaEdit(request);

        if(!ImageResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMessage());
        }

    }

}
