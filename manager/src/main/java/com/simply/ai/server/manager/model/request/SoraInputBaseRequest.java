package com.simply.ai.server.manager.model.request;

import com.simply.ai.server.manager.enums.SoraAspectRatioEnum;
import com.simply.ai.server.manager.enums.SoraFramesEnum;
import lombok.Data;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

@Data
public class SoraInputBaseRequest {

    @NotBlank(message = "提示词不能为空")
    @Size(max = 5000, message = "提示词长度不能超过5000个字符")
    private String prompt;

    private SoraAspectRatioEnum aspectRatio;

    private SoraFramesEnum nFrames;

    private Boolean removeWatermark;
}