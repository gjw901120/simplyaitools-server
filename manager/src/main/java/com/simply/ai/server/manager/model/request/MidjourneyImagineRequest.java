package com.simply.ai.server.manager.model.request;

import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;
import java.io.Serial;
import java.util.List;

/**
 * Imagine 任务请求参数
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class MidjourneyImagineRequest extends MidjourneyBaseRequest {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 图像生成的文本提示词
     */
    @NotBlank(message = "提示词不能为空")
    @Size(max = 2000, message = "提示词长度不能超过2000个字符")
    private String prompt;

    /**
     * 垫图的 base64 编码数组
     */
    @Size(max = 5, message = "垫图数量不能超过5张")
    private List<@NotBlank String> base64Array;

    /**
     * 构建基础 Imagine 请求
     */
    public static MidjourneyImagineRequest build(String prompt) {
        MidjourneyImagineRequest request = new MidjourneyImagineRequest();
        request.setPrompt(prompt);
        return request;
    }

    /**
     * 流式构建方法
     */
    public static MidjourneyImagineRequest create(String prompt) {
        return new MidjourneyImagineRequest().withPrompt(prompt);
    }

    /**
     * 设置提示词
     */
    public MidjourneyImagineRequest withPrompt(String prompt) {
        this.prompt = prompt;
        return this;
    }

    /**
     * 设置垫图
     */
    public MidjourneyImagineRequest withBase64Array(List<String> base64Array) {
        this.base64Array = base64Array;
        return this;
    }

    /**
     * 添加单个垫图
     */
    public MidjourneyImagineRequest addBase64Image(String base64Image) {
        if (this.base64Array == null) {
            this.base64Array = new java.util.ArrayList<>();
        }
        this.base64Array.add(base64Image);
        return this;
    }

    /**
     * 验证业务规则
     */
    public void validateBusinessRules() {
        if (base64Array != null && base64Array.size() > 5) {
            throw new IllegalArgumentException("垫图数量不能超过5张");
        }
        if (prompt == null || prompt.trim().isEmpty()) {
            throw new IllegalArgumentException("提示词不能为空");
        }
    }
}