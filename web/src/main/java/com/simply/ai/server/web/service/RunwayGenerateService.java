package com.simply.ai.server.web.service;

import com.simply.ai.server.web.model.dto.request.RunwayAlephDTO;
import com.simply.ai.server.web.model.dto.request.RunwayExtendDTO;
import com.simply.ai.server.web.model.dto.request.RunwayGenerateDTO;
import com.simply.ai.server.web.model.dto.response.BaseResponse;

public interface RunwayGenerateService {

    BaseResponse runwayGenerate(RunwayGenerateDTO request);

    BaseResponse runwayExtend(RunwayExtendDTO request);

    BaseResponse runwayAleph(RunwayAlephDTO request);

}
