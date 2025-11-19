package com.simply.ai.server.web.service.impl;

import com.simply.ai.server.manager.enums.*;
import com.simply.ai.server.manager.manager.SoraManager;
import com.simply.ai.server.manager.model.request.*;
import com.simply.ai.server.manager.model.response.VideoGenerateResponse;
import com.simply.ai.server.web.model.dto.request.*;
import com.simply.ai.server.web.service.SoraGenerateService;
import com.simply.common.core.exception.BaseException;
import com.simply.common.core.exception.error.ThirdpartyErrorType;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class SoraGenerateServiceImpl implements SoraGenerateService {

    @Autowired
    private SoraManager soraManager;

    @Override
    public void soraGenerate(SoraGenerateDTO soraGenerateDTO) {
        // 实现视频生成逻辑
        SoraGenerateRequest request = new SoraGenerateRequest();
        request.setModel(SoraModelEnum.getByCode(soraGenerateDTO.getModel()));

        if(soraGenerateDTO.getModel().equals(SoraModelEnum.SORA_2_TEXT_TO_VIDEO.getCode())) {
            SoraTextToVideoRequest soraRequest = new SoraTextToVideoRequest();
            soraRequest.setAspectRatio(SoraAspectRatioEnum.getByCode(soraGenerateDTO.getAspectRatio()));
            soraRequest.setPrompt(soraGenerateDTO.getPrompt());
            soraRequest.setNFrames(SoraFramesEnum.getByCode(soraGenerateDTO.getNFrames()));
            soraRequest.setRemoveWatermark(soraGenerateDTO.getRemoveWatermark());
            request.setInput(soraRequest);
        } else {
            SoraImageToVideoRequestRequest soraRequest = new SoraImageToVideoRequestRequest();
            soraRequest.setAspectRatio(SoraAspectRatioEnum.getByCode(soraGenerateDTO.getAspectRatio()));
            soraRequest.setPrompt(soraGenerateDTO.getPrompt());
            soraRequest.setNFrames(SoraFramesEnum.getByCode(soraGenerateDTO.getNFrames()));
            soraRequest.setRemoveWatermark(soraGenerateDTO.getRemoveWatermark());
            List<String> imageUrls = new ArrayList<>();
            soraRequest.setImageUrls(imageUrls);
            request.setInput(soraRequest);
        }

        VideoGenerateResponse response = soraManager.generateVideo(request);

        if(!ResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

    }


    @Override
    public void soraProGenerate(SoraProGenerateDTO soraProGenerateDTO) {
        // 实现视频生成逻辑
        SoraGenerateRequest request = new SoraGenerateRequest();
        request.setModel(SoraModelEnum.getByCode(soraProGenerateDTO.getModel()));

        if(soraProGenerateDTO.getModel().equals(SoraModelEnum.SORA_2_PRO_TEXT_TO_VIDEO.getCode())) {
            SoraProTextToVideoRequestRequest soraRequest = new SoraProTextToVideoRequestRequest();
            soraRequest.setAspectRatio(SoraAspectRatioEnum.getByCode(soraProGenerateDTO.getAspectRatio()));
            soraRequest.setPrompt(soraProGenerateDTO.getPrompt());
            soraRequest.setNFrames(SoraFramesEnum.getByCode(soraProGenerateDTO.getNFrames()));
            soraRequest.setRemoveWatermark(soraProGenerateDTO.getRemoveWatermark());
            soraRequest.setSize(SoraSizeEnum.getByCode(soraProGenerateDTO.getSize()));
            request.setInput(soraRequest);
        } else {
            SoraProImageToVideoRequestRequest soraRequest = new SoraProImageToVideoRequestRequest();
            soraRequest.setAspectRatio(SoraAspectRatioEnum.getByCode(soraProGenerateDTO.getAspectRatio()));
            soraRequest.setPrompt(soraProGenerateDTO.getPrompt());
            soraRequest.setNFrames(SoraFramesEnum.getByCode(soraProGenerateDTO.getNFrames()));
            soraRequest.setRemoveWatermark(soraProGenerateDTO.getRemoveWatermark());
            soraRequest.setSize(SoraSizeEnum.getByCode(soraProGenerateDTO.getSize()));
            List<String> imageUrls = new ArrayList<>();
            soraRequest.setImageUrls(imageUrls);
            request.setInput(soraRequest);
        }

        VideoGenerateResponse response = soraManager.generateVideo(request);

        if(!ResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

    }


    @Override
    public void soraWatermarkRemover(SoraWatermarkRemoverDTO soraWatermarkRemoverDTO) {
        // 实现视频生成逻辑
        SoraGenerateRequest request = new SoraGenerateRequest();

        SoraWatermarkRemoverRequest soraRequest = new SoraWatermarkRemoverRequest();

        request.setModel(SoraModelEnum.getByCode(soraWatermarkRemoverDTO.getModel()));

        soraRequest.setVideoUrl(soraWatermarkRemoverDTO.getVideoUrl());

        request.setInput(soraRequest);

        VideoGenerateResponse response = soraManager.soraWatermarkRemover(request);

        if(!ResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

    }


    @Override
    public void soraProStoryboard(SoraProStoryboardDTO soraProStoryboardDTO) {
        // 实现视频生成逻辑
        SoraGenerateRequest request = new SoraGenerateRequest();

        SoraStoryboardRequest soraRequest = new SoraStoryboardRequest();

        soraRequest.setAspectRatio(SoraAspectRatioEnum.getByCode(soraProStoryboardDTO.getAspectRatio()));
        soraRequest.setNFrames(SoraFramesEnum.getByCode(soraProStoryboardDTO.getNFrames()));
        List<String> imageUrls = new ArrayList<>();
        soraRequest.setImageUrls(imageUrls);

        List<SoraStoryboardSceneRequest> shots = new ArrayList<>();

        BeanUtils.copyProperties(shots, soraProStoryboardDTO.getShots());

        soraRequest.setShots(shots);

        request.setModel(SoraModelEnum.getByCode(soraProStoryboardDTO.getModel()));

        request.setInput(soraRequest);

        VideoGenerateResponse response = soraManager.soraStoryboard(request);

        if(!ResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

    }
}
