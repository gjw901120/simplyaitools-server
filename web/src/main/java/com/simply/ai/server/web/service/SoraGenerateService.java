package com.simply.ai.server.web.service;

import com.simply.ai.server.web.model.dto.request.*;

public interface SoraGenerateService {

    void soraGenerate(SoraGenerateDTO request);

    void soraProGenerate(SoraProGenerateDTO request);

    void soraWatermarkRemover(SoraWatermarkRemoverDTO request);

    void soraProStoryboard(SoraProStoryboardDTO request);
}
