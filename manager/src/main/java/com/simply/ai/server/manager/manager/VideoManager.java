package com.simply.ai.server.manager.manager;

import com.simply.ai.server.manager.model.request.*;
import com.simply.ai.server.manager.model.response.VideoGenerateResponse;

public interface VideoManager {

    VideoGenerateResponse generateVideo(VeoGenerateRequest request);
    VideoGenerateResponse generateVideo(RunwayGenerateRequest request);

    VideoGenerateResponse transformVideo(RunwayAlephGenerateRequest request);

    VideoGenerateResponse modifyVideo(LumaGenerateRequest request);

    VideoGenerateResponse extendVideo(VeoExtendRequest request);
}
