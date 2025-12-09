package com.simply.ai.server.web.service;

import com.simply.ai.server.web.model.dto.request.video.SoraGenerateDTO;
import com.simply.ai.server.web.model.dto.request.video.SoraProGenerateDTO;
import com.simply.ai.server.web.model.dto.request.video.SoraProStoryboardDTO;
import com.simply.ai.server.web.model.dto.request.video.SoraWatermarkRemoverDTO;
import com.simply.ai.server.web.model.dto.response.BaseResponse;

public interface SoraGenerateService {

    BaseResponse soraGenerate(SoraGenerateDTO request);

    BaseResponse soraProGenerate(SoraProGenerateDTO request);

    BaseResponse soraWatermarkRemover(SoraWatermarkRemoverDTO request);

    BaseResponse soraProStoryboard(SoraProStoryboardDTO request);
}
