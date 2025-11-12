package com.simply.ai.server.manager.manager;

import com.simply.ai.server.manager.model.request.VeoGenerateRequest;
import com.simply.ai.server.manager.model.response.VideoGenerateResponse;

public interface VideoManager {

    public VideoGenerateResponse generateVideo(VeoGenerateRequest request);
}
