package com.simply.ai.server.manager.manager;

import com.simply.ai.server.manager.model.request.*;
import com.simply.ai.server.manager.model.response.VideoGenerateResponse;

public interface VideoManager {

    VideoGenerateResponse veoGenerate(VeoGenerateRequest request);
    VideoGenerateResponse runwayGenerate(RunwayGenerateRequest request);

    VideoGenerateResponse runwayExtend(RunwayExtendRequest request);

    VideoGenerateResponse runwayAlephGenerate(RunwayAlephGenerateRequest request);

    VideoGenerateResponse lumaModify(LumaGenerateRequest request);

    VideoGenerateResponse veoExtend(VeoExtendRequest request);
}
