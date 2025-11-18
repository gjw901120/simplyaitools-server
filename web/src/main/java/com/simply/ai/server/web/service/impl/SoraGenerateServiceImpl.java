package com.simply.ai.server.web.service.impl;

import com.simply.ai.server.manager.enums.ResponseCodeEnum;
import com.simply.ai.server.manager.manager.SoraManager;
import com.simply.ai.server.manager.model.request.SoraGenerateRequest;
import com.simply.ai.server.manager.model.response.VideoGenerateResponse;
import com.simply.ai.server.web.model.dto.request.*;
import com.simply.ai.server.web.service.SoraGenerateService;
import com.simply.common.core.exception.BaseException;
import com.simply.common.core.exception.error.ThirdpartyErrorType;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SoraGenerateServiceImpl implements SoraGenerateService {

    @Autowired
    private SoraManager soraManager;

    @Override
    public void soraGenerate(SoraGenerateDTO soraGenerateDTO) {
        // 实现视频生成逻辑
        SoraGenerateRequest request = new SoraGenerateRequest();

        BeanUtils.copyProperties(soraGenerateDTO, request);

        VideoGenerateResponse response = soraManager.generateVideo(request);

        if(!ResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

    }


    @Override
    public void soraProGenerate(SoraProGenerateDTO soraProGenerateDTO) {
        // 实现视频生成逻辑
        SoraGenerateRequest request = new SoraGenerateRequest();

        BeanUtils.copyProperties(soraProGenerateDTO, request);

        VideoGenerateResponse response = soraManager.generateVideo(request);

        if(!ResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

    }


    @Override
    public void soraWatermarkRemover(SoraWatermarkRemoverDTO soraWatermarkRemoverDTO) {
        // 实现视频生成逻辑
        SoraGenerateRequest request = new SoraGenerateRequest();

        BeanUtils.copyProperties(soraWatermarkRemoverDTO, request);

        VideoGenerateResponse response = soraManager.soraWatermarkRemover(request);

        if(!ResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

    }


    @Override
    public void soraProStoryboard(SoraProStoryboardDTO soraProStoryboardDTO) {
        // 实现视频生成逻辑
        SoraGenerateRequest request = new SoraGenerateRequest();

        BeanUtils.copyProperties(soraProStoryboardDTO, request);

        VideoGenerateResponse response = soraManager.soraStoryboard(request);

        if(!ResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

    }
}
