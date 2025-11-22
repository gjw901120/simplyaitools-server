package com.simply.ai.server.web.service;

import com.simply.ai.server.web.model.dto.request.*;
import com.simply.ai.server.web.model.dto.response.BaseResponse;

public interface SoraGenerateService {

    BaseResponse soraGenerate(SoraGenerateDTO request);

    BaseResponse soraProGenerate(SoraProGenerateDTO request);

    BaseResponse soraWatermarkRemover(SoraWatermarkRemoverDTO request);

    BaseResponse soraProStoryboard(SoraProStoryboardDTO request);
}
