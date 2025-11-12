package com.simply.ai.server.manager.manager.impl;

import com.simply.ai.server.manager.feign.client.VideoFeignClient;
import com.simply.ai.server.manager.manager.VideoManager;
import com.simply.ai.server.manager.model.request.VeoGenerateRequest;
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
}