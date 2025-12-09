package com.simply.ai.server.web.service;

import com.simply.ai.server.web.model.dto.request.video.LumaGenerateDTO;
import com.simply.ai.server.web.model.dto.response.BaseResponse;

public interface LumaGenerateService {

    BaseResponse lumaGenerate(LumaGenerateDTO request);
}
