package com.simply.ai.server.web.service;

import com.simply.ai.server.web.model.dto.request.RunwayAlephDTO;
import com.simply.ai.server.web.model.dto.request.RunwayExtendDTO;
import com.simply.ai.server.web.model.dto.request.RunwayGenerateDTO;

public interface RunwayGenerateService {

    void runwayGenerate(RunwayGenerateDTO request);

    void runwayExtend(RunwayExtendDTO request);

    void runwayAleph(RunwayAlephDTO request);

}
