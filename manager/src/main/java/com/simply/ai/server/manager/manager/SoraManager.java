package com.simply.ai.server.manager.manager;

import com.simply.ai.server.manager.model.request.SoraGenerateRequest;
import com.simply.ai.server.manager.model.response.VideoGenerateResponse;


public interface SoraManager {

    VideoGenerateResponse generateVideo(SoraGenerateRequest request);

    VideoGenerateResponse soraWatermarkRemover(SoraGenerateRequest request);

    VideoGenerateResponse soraStoryboard(SoraGenerateRequest request);

}
