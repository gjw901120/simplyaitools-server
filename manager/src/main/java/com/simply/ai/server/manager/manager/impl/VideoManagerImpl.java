package com.simply.ai.server.manager.manager.impl;

import com.simply.ai.server.manager.feign.client.VideoFeignClient;
import com.simply.ai.server.manager.manager.VideoManager;
import com.simply.ai.server.manager.model.request.*;
import com.simply.ai.server.manager.model.response.VideoGenerateResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;


@Service
public class VideoManagerImpl implements VideoManager {

    @Autowired
    private VideoFeignClient videoFeignClient;

    @Override
    public VideoGenerateResponse generateVideo(VeoGenerateRequest request) {
        return videoFeignClient.generateVideo(request);
    }

    @Override
    public VideoGenerateResponse generateVideo(RunwayGenerateRequest request) {
        return videoFeignClient.generateVideo(request);
    }

    @Override
    public VideoGenerateResponse transformVideo(RunwayAlephGenerateRequest request) {
        return videoFeignClient.transformVideo(request);
    }

    @Override
    public VideoGenerateResponse modifyVideo(LumaGenerateRequest request) {
        return videoFeignClient.modifyVideo(request);
    }

    @Override
    public VideoGenerateResponse extendVideo(VeoExtendRequest request) {
        return videoFeignClient.extendVideo(request);
    }
}